classdef crcCommon
    
    
    properties
        start_date = 45;
        
    end
    
    
    methods
        
        
        function theta = crc(obj, dataFilename,  prefix, draw)
            
            theta = [];
            [num, txt, raw] = xlsread(dataFilename);
            
            %n_rows: number of days for covid
            %n_cols: number of days with public reports
            [n_rows, n_cols] = size(num);
            num_cases_per_day = 30; % to be considered, a day has to have more than 30 cases
            k =obj.start_date;
            dk = n_rows - 31;
            %save the observations with about completeness of reports
            V = []; %total confirmed positives
            m = 1;
            valid = 1;
            if draw == 1
                figure(1000)
                clf
                hold on
            end
            for i=k:dk %range of interest
                %disp([i,k])
                %disp(t)
                v = num(i, m:end);
                if draw == 1
                    plot(v,'linewidth',2)
                end
                if max(v) > num_cases_per_day
                    V(valid,1:length(v)) =  v;
                    valid = valid + 1;
                end
                m = m + 1;
            end
            if draw == 1
                hold off
            end
            
            if not(isempty(V))
                %completa el maximo para las lecturas inexistentes, aquellas
                %que aun no llegan
                [v_rows, v_cols] = size(V);
                %v_rows, number of days with observation complete
                %v_cols, number of days with report
                for i=1:v_rows
                    maximo = max(V(i,:));
                    posicion = min(find(V(i,:)== maximo));
                    %disp(posicion)
                    %fill up the gaps
                    V(i,posicion:end) = maximo;
                end
                
                %            figure(100)
                %            plot(V','linewidth',2)
                %            set(gca, 'FontSize', 16)
                %            xlabel('$t$',  'Interpreter','LaTex','FontSize', 16)
                %            ylabel('$c_d(t)$',  'Interpreter','LaTex','FontSize', 16)
                %
                %compute the compound rate of change
                for t=1:v_rows %day observed, just the first dk days seem to converge
                    now = max(V(t,:));
                    for delta = 1:v_cols %init time
                        before = V(t,delta);
                        
                        rho(t, delta) = (now - before)/before;
                        
                    end
                    
                end
                num_sequences_complete = 30;
                %fit distributions
                [r_rows, r_cols] = size(rho);
                m = 1;bins = 40;
                for delta=1:r_cols
                    data = rho(:,delta);
                    %data_p = reshape(data,[length(data),1]);
                    indx = find(not(data == Inf) & not(data == 0));
                    %disp(length(indx))
                    if length(indx)>num_sequences_complete
                        
                        phat  = gamfit(data(indx));
                        theta(delta,:) = phat;
                        x = linspace(min(data(indx)),max(data(indx)),bins);
                        y = gampdf(x,phat(1), phat(2) );
                        y = y/sum(y);
                        
                        if draw == 1
                            figure(m)
                            histogram(data,bins, 'Normalization', 'probability')
                            hold on
                            scatter(x,y,100,'fill');
                            plot(x,y,'linewidth',2)
                            hold off
                            
                            set(gca, 'FontSize', 16)
                            xlabel('$\rho$',  'Interpreter','LaTex','FontSize', 16)
                            ylabel('$p(\rho)$',  'Interpreter','LaTex','FontSize', 16)
                            cadena = sprintf('$\\delta$ = %d', delta-1);
                            title(cadena,  'Interpreter','LaTex','FontSize', 16)
                            filename = sprintf('../figures/20200713-%03d%s.png', delta-1,prefix);
                            saveas(gcf, filename);
                            m = m + 1;
                        end
                    end
                end
                if not(isempty(theta))
                    %https://www.mathworks.com/matlabcentral/answers/467038-how-to-add-headers-to-excel
                    header = {'shape', 'scale'}; %dummy header
                    c_theta = cell(size(theta,1)+1, size(theta,2));
                    c_theta(1,:) = header;
                    c_theta(2:size(theta,1)+1,:) = num2cell(theta);
                    
                    filename = sprintf('../data/GammaParam%s.csv', prefix);
                    csvwrite(filename, theta);
                end
            end
        end
        
        
        %using current model compute plausible escenarios
        function [q0_025, q0_5, q0_975] = predict(obj, theta, day, before)
            %disp(day)
            if before > 0
                phat = theta(day,:);
                a = phat(1); b = phat(2);
                if a > 0 & b > 0
                    rho = gamrnd(a,b,[1,1000]);
                    y = quantile(before * (1 + rho),[0.025  0.50 0.975]);
                    q0_025 = y(1);
                    q0_5 =  y(2);
                    q0_975 =  y(3);
                else
                    q0_025 = inf;
                    q0_5 =  inf;
                    q0_975 =  inf;
                    
                end
            else
                q0_025 = inf;
                q0_5 =  inf;
                q0_975 =  inf;
            end
        end
        
        
        %obtain samples for the current situation
        function sampling(obj, datafilename, prefix, theta,offset)
            [num, txt, raw] = xlsread(datafilename);
            
            %n_rows: number of days for covid
            %n_cols: number of days with public reports
            [n_rows, n_cols] = size(num);
            n_samples = 1000;
            k =obj.start_date;
            %dk = 59;
            %save the observations with about completeness of reports
            Q = zeros(n_rows, n_samples);
            C = max(num,[],2);
            
            
            for t=1:n_rows
                Q(t,:) = C(t);
            end
            n = k;
            m = 1;
            for t=k:(n_rows-offset)
                %disp([i,n_rows, size(num,2) - n+1])
                %disp(t)
                v = num(t, m:end);
                %delta = max(find(v == 0));
                day =  size(num,2) - m+1; %one more because matlab arrays are base one
                %if delta > 0
                
                c = max(v);
                %day = length(v) - delta;
                if day < size(theta,1)
                    %disp([day, c])
                    [minimum,media, maximum] = obj.predict(theta, day, c);
                    if not(isinf(minimum))
                        a = theta(day,1); b = theta(day,2);
                        if a > 0 & b > 0
                            r = gamrnd(a,b,[1,1000]);
                            Q(n,:) = floor(c * (1 + r));
                            n = n + 1;
                        end
                    end
                    
                    
                else
                    %    Q(i,:) = C(i);
                    n = n + 1;
                end
                m = m + 1;
                %%end
                % n = n + 1;
            end
            Q = Q(1:n-1,:);
            figure(1)
            clf
            plot(Q,'linewidth',1.5);
            
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('confirmed positives',  'Interpreter','LaTex','FontSize', 16)
            
            
            
            header = {}; %dummy header
            for t=1:n_samples
                num = num2str(t);
                header{t} = num;
            end
            c_Q = cell(size(Q,1)+1, size(Q,2)+1);
            c_Q(1,2:end) = header;
            c_Q(2:end,1) = txt(2:size(c_Q,1),1);
            c_Q(2:size(Q,1)+1, 2:size(Q,2)+1) = num2cell(Q);
            %axis([0,135,0,3e4])
            
            filename = sprintf('../data/infectious_samples%s.xlsx', prefix);
            if exist(filename, 'file');
                delete(filename);
            end
            
            xlswrite(filename, c_Q)
            
            
            
        end
        
        
        
        function nowcasting(obj, datafilename, theta, offset)
            [num, txt, raw] = xlsread(datafilename);
            
            %n_rows: number of days for covid
            %n_cols: number of days with public reports
            [n_rows, n_cols] = size(num);
            
            k =obj.start_date;
            %dk = 59;
            %save the observations with about completeness of reports
            
            C = max(num,[],2);
            m = 1; n = 1;
            for t=k:(n_rows-offset)
                %disp([i,k])
                %disp(t)
                v = num(t, m:end);
                pos = max(find(v == 0));
                %day = length(v) - pos;
                day =  size(num,2) - n+1; %one more because matlab arrays are base one
                
                c = max(v);
                
                if day < size(theta,1)
                    %disp([day, c])
                    [minimum,media, maximum] = obj.predict(theta, day, c);
                    %disp(maximum - minimum)
                    if not(isinf(minimum))
                        Q(m,:) = [t, minimum, media, maximum];
                        m = m + 1;
                    end
                end
                n = n + 1;
            end
            figure(2)
            clf
            plot(C,'linewidth',2);
            hold on
            tiempo = Q(:,1)'-2;
            lim_inf = Q(:,2)';
            valor_medio = Q(:,3)';
            lim_sup = Q(:,4)';
            %plot(tiempo, lim_inf,'k')
            
            %plot(tiempo, lim_sup,'k')
            
            patch([tiempo fliplr(tiempo)], [  valor_medio fliplr(lim_inf)], [0.5 0.5 0.5], 'EdgeColor','none')  % Plot +0.1 C Band
            
            patch([tiempo fliplr(tiempo)], [  valor_medio fliplr(lim_sup)], [0.5 0.5 0.5], 'EdgeColor','none')
            
            plot(tiempo, valor_medio,'r','linewidth',2)
            hold off
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('confirmed positives',  'Interpreter','LaTex','FontSize', 16)
            axis([0,n_rows+1,0,max(lim_sup)])
            
            
            
        end
        
        
        
        
        
    end
    
end

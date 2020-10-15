classdef crcCommonNoParam
    
    
    properties
        start_date = 45;
        
    end
    
    
    methods
        
        
        function theta = crc(obj, dataFilename,  prefix, draw)
            Domain = {};
            Prob = {};
            theta = [];
            [num, txt, raw] = xlsread(dataFilename);
            
            %n_rows: number of days for covid
            %n_cols: number of days with public reports
            [n_rows, n_cols] = size(num);
            num_cases_per_day = 20; % to be considered, a day has to have more than 30 cases
            k =obj.start_date;
            dk = n_rows - 31; %I assume that 
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
                if draw == 1
                    figure(100)
                    plot(V','linewidth',2)
                    set(gca, 'FontSize', 16)
                    xlabel('$d$',  'Interpreter','LaTex','FontSize', 16)
                    ylabel('$c_t(d)$',  'Interpreter','LaTex','FontSize', 16)
                    
                end
                %compute the compound rate of change
                for t=1:v_rows %day observed, just the first dk days seem to converge
                    now = max(V(t,:));
                    for delta = 1:v_cols %init time
                        before = V(t,delta);
                        
                        rho(t, delta) = (now - before)/before;
                        
                    end
                    
                end
                num_sequences_complete = 19;
                %fit distributions
                [r_rows, r_cols] = size(rho);
                z = 1; m = 1;bins = 40;
                for delta=1:r_cols
                    data = rho(:,delta);
                    %data_p = reshape(data,[length(data),1]);
                    indx = find(not(data == Inf) & not(data == 0));
                    %disp(length(indx))
                    if length(indx)>= num_sequences_complete
                        
                        phat  = gamfit(data(indx));
                        theta(delta,:) = phat;
                        x = linspace(min(data(indx)),max(data(indx)),bins);
                        y = gampdf(x,phat(1), phat(2) );
                        y = y/sum(y);
                        [p_h,rho_h] = histcounts(data, bins,'Normalization', 'cdf');
                        rho_h_2 = (rho_h(2:end) + rho_h(1:end-1))/2;
                        
                        t_rho = (sort(data(not(isinf(data)))));
                        Domain(delta) =   {t_rho};
                        t_n = length(t_rho);
                        t_x = (0:(t_n-1))/t_n;
                        Prob(delta) =  {t_x};
                        if draw == 1
                            figure(m)
                            histogram(data,bins, 'Normalization', 'probability')
                            %histogram(data,bins, 'Normalization', 'cdf')
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
                        z = z + 1;
                    end
                end
                disp(z)
                if not(isempty(theta))
                    %https://www.mathworks.com/matlabcentral/answers/467038-how-to-add-headers-to-excel
                    header = {'shape', 'scale'}; %dummy header
                    c_theta = cell(size(theta,1)+1, size(theta,2));
                    c_theta(1,:) = header;
                    c_theta(2:size(theta,1)+1,:) = num2cell(theta);
                    
                    %filename = sprintf('../data/GammaParam%s.csv', prefix);
                    %csvwrite(filename, theta);
                    filename = sprintf('../data/NoParam%s.mat', prefix);
                    %filename = sprintf('../data/domain%s.csv', prefix);
                    %csvwrite(filename, Domain);
                    %filename = sprintf('../data/probability%s.csv', prefix);
                    %csvwrite(filename, Prob);
                    save(filename,'Domain','Prob');
                end
            end
        end
        
        
        %using current model compute plausible escenarios
        function [q0_025, q0_5, q0_975] = predict(obj, theta, day, before)
            %disp(day)
            if before > 0
                phat = theta(day+1,:); %add one because the indices in matlab
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
        
        
        %using current model compute plausible escenarios
        function [q0_025, q0_5, q0_975] = predict2(obj, Domain, Prob, day, before)
            q0_025 = inf; q0_5 = inf; q0_975 = inf;
            %disp(day)
            if before > 0
                p_h = Prob{day+1};
                rho_h = Domain{day+1};
                if not(isempty(p_h))
                    u = rand(1,1000);
                    %u = rand(1000,1);
                    rho = interp1(p_h, rho_h, u);
                    num_missing = sum(isnan(rho));
                    while num_missing > 0
                        indx = find(isnan(rho));
                        rho(indx) = interp1(p_h, rho_h, rand(1,length(indx)));
                        num_missing = sum(isnan(rho));
                    end
                    
                    
                    %                 rho =[];
                    %                 for i=1:1000
                    %                     pos = min(find( p_h >= u(i) ));
                    %                     if isempty(pos)
                    %                         pos = length(rho_h);
                    %                     end
                    %                     rho(i) = rho_h(pos);
                    %                 end
                    y = quantile(before * (1 + rho),[0.025  0.50 0.975]);
                    q0_025 = y(1);
                    q0_5 =  y(2);
                    q0_975 =  y(3);
                end
                
             end
        end
        
        
        
        %obtain samples for the current situation
        function sampling(obj, datafilename, prefix, theta,offset)
            
             filename = sprintf('../data/NoParam%s.mat', prefix);
            %filename = sprintf('../data/domain%s.csv', prefix);
            %csvwrite(filename, Domain);
            %filename = sprintf('../data/probability%s.csv', prefix);
            %csvwrite(filename, Prob);
            load(filename,'Domain','Prob');
             
            
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
            Q = C * ones(1, n_samples);
            
            n = k;
            m = 1;
            %check every day of the infectious between the start of reporting <abril 12> (k days after
            % february 28) and <offset> days before today
            for t=k:(n_rows-offset)
                %disp([i,n_rows, size(num,2) - n+1])
                %disp(t)
                v = num(t, m:end);
                %delta = max(find(v == 0));
                delta =  size(num,2) - m; %one more because matlab arrays are base one
                                          %2020.08.12 removed the addition
                                          %of one. When m == size(num,2),
                                          %delta must be equal to zero.
  
                c = max(v); %it may not be the last day due to dropped cases
               
                if delta < size(theta,1)
                    %disp([day, c])
                    %<day> corresponding to today is zero. I add one to <day>
                    %in predict
                    [minimum,media, maximum] = obj.predict2(Domain, Prob, delta, c);
                    if not(isinf(minimum))
                        p_h = Prob{delta+1};
                        rho_h = Domain{delta+1};
                        u = rand(1,1000);
                        rho = interp1(p_h, rho_h, u);
                        
                        num_missing = sum(isnan(rho));
                        while num_missing > 0
                            indx = find(isnan(rho));
                            rho(indx) = interp1(p_h, rho_h, rand(1,length(indx)));
                            num_missing = sum(isnan(rho));
                        end
                
%                         rho =[];
%                         for i=1:1000
%                             pos = min(find( p_h >= u(i) ));
%                             if isempty(pos)
%                                 pos = length(rho_h);
%                             end
%                             rho(i) = rho_h(pos);
%                         end
%                         
                        Q(n,:) = floor(c * (1 + rho));
                        n = n + 1;
                    end
                else
                    n = n + 1;
                end
                m = m + 1; 
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
        
        
        
        function nowcasting(obj, datafilename, theta, offset, prefix)
            
            
            filename = sprintf('../data/NoParam%s.mat', prefix);
            %filename = sprintf('../data/domain%s.csv', prefix);
            %csvwrite(filename, Domain);
            %filename = sprintf('../data/probability%s.csv', prefix);
            %csvwrite(filename, Prob);
            load(filename,'Domain','Prob');
                    
            %filename = sprintf('../data/domain%s.csv', prefix);
            %Domain = xlsread(filename);
            %filename = sprintf('../data/probability%s.csv', prefix);
            %Prob = xlsread(filename);
            
            
                    
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
%                 if t == 222
%                 disp([t,k])
%                 end
                
                %disp(t)
                v = num(t, m:end);
                pos = max(find(v == 0));
                 
                delta =  size(num,2) - n; %one more because matlab arrays are base one
                                        %I think adding one more is a mistake,
                                        %I removed the addition. JSR.
                                        %2020.07.27
                                        %2020.08.12 reviewed. When n =
                                        %size(num,2), then delta must be
                                        %equal to zero
                                        
                
                c = max(v);
                
                if delta < size(theta,1)
                    %disp([day, c])
                    %[minimum,media, maximum] = obj.predict(theta, delta, c);
                    [minimum,media, maximum] = obj.predict2(Domain, Prob, delta, c);
                    %disp([minimum, maximum])
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
            scatter(1:length(C),C,10,'fill');
            tiempo = Q(:,1)'-2;
            lim_inf = Q(:,2)';
            valor_medio = Q(:,3)';
            lim_sup = Q(:,4)';
            %plot(tiempo, lim_inf,'k')
            
            %plot(tiempo, lim_sup,'k')
            
            patch([tiempo fliplr(tiempo)], [  valor_medio fliplr(lim_inf)], [0.5 0.5 0.5], 'EdgeColor','none')  % Plot +0.1 C Band
            
            patch([tiempo fliplr(tiempo)], [  valor_medio fliplr(lim_sup)], [0.5 0.5 0.5], 'EdgeColor','none')
            
            %plot(tiempo, valor_medio,'r','linewidth',2)
            hold off
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('confirmed positives',  'Interpreter','LaTex','FontSize', 16)
            axis([0.75*n_rows,n_rows+1,0,max(lim_sup)])
            
            
            header = {'fecha','r0.025', 'mean', 'r0.975'}; %dummy header
            %for t=1:n_samples
            %    num = num2str(t);
            %    header{t} = num;
            %end
            c_Q = cell(size(Q,1)+1, size(Q,2));
            c_Q(1,1:end) = header;
            c_Q(2:(length(tiempo)+1),1) = txt(tiempo+1,1);
            c_Q(2:size(Q,1)+1, 2:size(Q,2)) = num2cell(Q(:,2:end));
            %axis([0,135,0,3e4])
            
            filename = sprintf('../data/nowcasting%s.xlsx', prefix);
            if exist(filename, 'file');
                delete(filename);
            end
            
            xlswrite(filename, c_Q)
            
            
        end
        
        
        
        
        
    end
    
end

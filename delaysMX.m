

%plotDelays
%process
%compoundRateOfChange2
%nowcastingCompound(theta)
%nowcastingSamples(theta)
classdef delaysMX
    
    
    properties
        num_max_days= 35 %num max of days to generate data
        masterFile
        prefixes ;
    end
    
    methods
        
        
        function obj = delaysMX
            obj.prefixes = {'AS','BC','BS','CC','CL','CM','CS','CH','DF',...
                'DG','GT','GR','HG','JC','MC','MN','MS','NT','NL',...
                'OC','PL','QT','QR','SP','SL','SR','TC','TS','TL',...
                'VZ','YN','ZS'}; 
        end
        
        
        %plot delays
        % take the confirmed positives by start of symptoms date
        %for the report issued each date
        function num = plotDelays(obj)
            
            [num, txt, raw] = xlsread(obj.masterFile);
            
            [m,n] = size(num);
            %num = cumsum(num,2);
            figure(1)
            clf
            hold on
            %start from the end to  highlight the furthest
            for i=n:-1:1
                area(num(:,i),'EdgeColor','flat')%,'linewidth',2);
            end
            hold off
            
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('positives',  'Interpreter','LaTex','FontSize', 16)
            axis([1,m,0, max(num(:))])
            
            
            axis([m-20,m,0, max(num(:))])
            
        end
        
        
        
        
        
        
        
        
        %get the model for each time tick
        function [mu,sigma]  = modelDelaysGaussian(obj, draw)
            load('data')
            %Y is a m x n matrix, where m contains the
            %day when  the report was issued and
            %n is the number of reports where the data
            %for a particular day have been changed
            for i=1:obj.num_max_days%size(Y,2)
                indx = find(Y(:,i)> 0);
                %we will analyze only the first 25 columns, why?
                %if i <= 25
                %take the data that is positive
                u = Y(indx,i);
                if draw == 1
                    figure(1)
                    clf
                    histogram(u,length(indx),'Normalization', 'probability');
                    hold on
                end
                %fit a normal distribution to the data
                [m,s] = normfit(u);
                
                mu(i) = m;
                sigma(i) = s;
                
                x = linspace(min(u), max(u),40);
                y = normpdf(x,m, s);
                
                y = y/sum(y);
                
                
                if draw == 1
                    scatter(x, y,50,'fill' )
                    hold off
                    
                    set(gca, 'FontSize', 16)
                    xlabel('$x^k/w_t$',  'Interpreter','LaTex','FontSize', 16)
                    ylabel('$p(x^k\mid w_t)$',  'Interpreter','LaTex','FontSize', 16)
                end
                %else
                %    break;
                %end
                
            end
            csvwrite('../data/gaussian.csv',[mu',sigma'])
            
        end
        
        
        %get the model for each time tick
        function [mu, sigma]  = modelDelaysGamma(obj, draw)
            load('data')
            %Y is a m x n matrix, where m contains the
            %day when  the report was issued and
            %n is the number of reports where the data
            %for a particular day have been changed
            for i=1:obj.num_max_days%size(Y,2)
                indx = find(Y(:,i)> 0);
                %we will analyze only the first 25 columns, why?
                %if i <= 25
                %take the data that is positive
                u = Y(indx,i);
                if draw == 1
                    figure(1)
                    clf
                    histogram(u,length(indx),'Normalization', 'probability');
                    hold on
                end
                %fit a normal distribution to the data
                %[m,s] = normfit(u);
                phat  = gamfit(u); %phat(1) = shape,phat(2) = scale
                mu(i) = phat(1)*phat(2);
                sigma(i) = sqrt(phat(1))*phat(2);
                
                x = linspace(min(u), max(u),40);
                %y = normpdf(x,m, s);
                y = gampdf(x,phat(1), phat(2));
                y = y/sum(y);
                
                
                if draw == 1
                    scatter(x, y,50,'fill' )
                    hold off
                    
                    set(gca, 'FontSize', 16)
                    xlabel('$x^k/w_t$',  'Interpreter','LaTex','FontSize', 16)
                    ylabel('$p(x^k\mid w_t)$',  'Interpreter','LaTex','FontSize', 16)
                end
                %else
                %    break;
                %end
                
            end
            csvwrite('../data/gamma.csv',[mu',sigma'])
            
        end
        
        
        
        
        function  g  = gauss(obj, x, mu, sigma)
            f = exp(-0.5*((x-mu)/sigma).^2);
            g = f/sum(f);
        end
        
        
        
        function    process(obj)
            
            [num, txt, raw] = xlsread(obj.masterFile);
            
            [rows, cols] = size(num);
            
            figure(100)
            mesh(num)
            set(gca, 'FontSize', 16)
            ylabel('days reported',  'Interpreter','LaTex','FontSize', 16)
            xlabel('update occurs',  'Interpreter','LaTex','FontSize', 16)
            
            %ylabel('day reported',  'Interpreter','LaTex','FontSize', 16)
            %xlabel('update occurs',  'Interpreter','LaTex','FontSize', 16)
            %colormap(gray(256))
            view(-85,42)
            
            figure(101)
            [gx, gy] = gradient(num);
            imagesc(gx)
            set(gca, 'FontSize', 16)
            xlabel('difference between days released',  'Interpreter','LaTex','FontSize', 16)
            ylabel('day reported',  'Interpreter','LaTex','FontSize', 16)
            
            
            figure(102)
            ngx = gx./sum(gx,2);
            mesh(ngx)
            axis([1, size(ngx,2), 40, 80, 0, 0.15])
            set(gca, 'FontSize', 16)
            xlabel('difference between days released',  'Interpreter','LaTex','FontSize', 16)
            ylabel('day reported',  'Interpreter','LaTex','FontSize', 16)
            
            
            
            %num = cumsum(num,2);
            figure(1)
            clf
            
            figure(2)
            clf
            
            x = 1:cols;
            k = 19;
            t = 0:-1:-k;
            Y = [];
            V = []; %total confirmed positives
            m = 1;
            for i=rows- k + 1:-1:46
                %disp([i,k])
                %disp(t)
                
                y = ngx(rows-k, end-k:end);
                v = num(rows-k, end-k:end);
                c = rand(1,3);
                figure(1)
                hold on
                scatter(-t,y,10, c,'fill')%,'linewidth',2);
                plot(-t,y,'Color',c)%,'linewidth',2);
                hold off
                drawnow;
                
                
                
                figure(2)
                hold on
                %scatter(-t,y,10, c,'fill')%,'linewidth',2);
                plot(-t,cumsum(y),'.-','Color',c)%,'linewidth',2);
                hold off
                drawnow;
                
                
                
                t = [t, -(k+1)];
                k = k + 1;
                Y(m,1:length(y)) =  y;
                V(m,1:length(v)) =  v;
                m = m + 1;
                disp(y(end))
            end
            
            figure(1)
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('rate of change',  'Interpreter','LaTex','FontSize', 16)
            %axis([1,n,0, max(num(:))])
            axis([0, size(num,2), 0,0.20])
            
            
            figure(2)
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('cumulative',  'Interpreter','LaTex','FontSize', 16)
            %axis([1,n,0, max(num(:))])
            axis([0, size(num,2), 0,1.05])
            
            
            % figure(2)
            % plot(m, 'linewidth',2)
            % set(gca, 'FontSize', 16)
            % xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            % ylabel('mean offset',  'Interpreter','LaTex','FontSize', 16)
            %
            %
            
            save('data','Y', 'V')
            
            figure(200)
            mesh(Y(end:-1:1,:))
            view([20,60])
            set(gca, 'FontSize', 16)
            xlabel('update occurs',  'Interpreter','LaTex','FontSize', 16)
            ylabel('day reported',  'Interpreter','LaTex','FontSize', 16)
            %colormap(gray(256))
            yticks([0,10,20,30,40])
            yticklabels({'40','50','60','70','80'})
            
            figure(201)
            mesh(V(end:-1:1,:))
            view([20,60])
            set(gca, 'FontSize', 16)
            ylabel('day reported',  'Interpreter','LaTex','FontSize', 16)
            xlabel('update occurs',  'Interpreter','LaTex','FontSize', 16)
            %colormap(gray(256))
            yticks([0,10,20,30,40])
            yticklabels({'40','50','60','70','80'})
            view(-84,60)
        end
        
        
        
        
        function    compoundRateOfChange(obj)
            
            [num, txt, raw] = xlsread(obj.masterFile);
            
            %n_rows: number of days for covid
            %n_cols: number of days with public reports
            [n_rows, n_cols] = size(num);
            
            k =45;
            dk = 59;
            %save the observations with about completeness of reports
            V = []; %total confirmed positives
            m = 1;
            for i=k:k + dk -1
                %disp([i,k])
                %disp(t)
                v = num(i, m:end);
                V(m,1:length(v)) =  v;
                m = m + 1;
            end
            
            
            
            
            %completa el maximo para las lecturas inexistentes, aquellas
            %que aun no llegan
            [v_rows, v_cols] = size(V);
            %v_rows, number of days with observation complete
            %v_cols, number of days with report
            for i=1:v_rows
                maximo = max(V(i,:));
                posicion = find(V(i,:)== maximo);
                %fill up the gaps
                V(i,posicion:end) = maximo;
            end
            
            figure(100)
            plot(V','linewidth',2)
            set(gca, 'FontSize', 16)
            xlabel('$t$',  'Interpreter','LaTex','FontSize', 16)
            ylabel('$c_d(t)$',  'Interpreter','LaTex','FontSize', 16)
            
            %compute the compound rate of change
            for i=1:dk %day reported, just the first dk days seem to converge
                for t = 1:v_cols %init time
                    before = V(i,t);
                    for tau=1:v_cols %elapsed time
                        if t + tau <= v_cols
                            %for each day complete
                            now = V(i,t + tau);
                            
                            
                        else
                            now = max(V(i,:));
                        end
                        rho(t,tau, i) = (now - before)/before;
                    end
                end
                
            end
            
            %fit distributions
            [r_rows, r_cols,r_depth] = size(rho);
            m = 1;bins = 40;
            for t=2:r_rows
                data = rho(t,end,:);
                data_p = reshape(data,[length(data),1]);
                indx = find(not(data_p == Inf) & not(data_p == 0));
                phat  = gamfit(data_p(indx));
                x = linspace(min(data_p(indx)),max(data_p(indx)),bins);
                y = gampdf(x,phat(1), phat(2) );
                y = y/sum(y);
                figure(m)
                histogram(data_p,bins, 'Normalization', 'probability')
                hold on
                scatter(x,y,100,'fill');
                plot(x,y,'linewidth',2)
                hold off
                
                set(gca, 'FontSize', 16)
                xlabel('$\rho$',  'Interpreter','LaTex','FontSize', 16)
                ylabel('$p(\rho)$',  'Interpreter','LaTex','FontSize', 16)
                cadena = sprintf('$t$ = %d', t-1);
                title(cadena,  'Interpreter','LaTex','FontSize', 16)
                filename = sprintf('../figures/pdf%03d.png', t);
                saveas(gcf, filename);
                m = m + 1;
            end
            
        end
        
        
        
        
        function    theta = compoundRateOfChange2(obj, draw)
            theta = [];
            [num, txt, raw] = xlsread(obj.masterFile);
            
            %n_rows: number of days for covid
            %n_cols: number of days with public reports
            [n_rows, n_cols] = size(num);
            
            k =45;
            dk = n_rows - 31;
            %save the observations with about completeness of reports
            V = []; %total confirmed positives
            m = 1;
            for i=k:dk
                %disp([i,k])
                %disp(t)
                v = num(i, m:end);
                V(m,1:length(v)) =  v;
                m = m + 1;
            end
            
            
            
            
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
            for i=1:v_rows %day observed, just the first dk days seem to converge
                now = max(V(i,:));
                for t = 1:v_cols %init time
                    before = V(i,t);
                    
                    rho(i, t) = (now - before)/before;
                    
                end
                
            end
            
            %fit distributions
            [r_rows, r_cols] = size(rho);
            m = 1;bins = 40;
            for t=2:r_rows
                data = rho(:,t);
                %data_p = reshape(data,[length(data),1]);
                indx = find(not(data == Inf) & not(data == 0));
                phat  = gamfit(data(indx));
                theta(t-1,:) = phat;
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
                    cadena = sprintf('$t$ = %d', t);
                    title(cadena,  'Interpreter','LaTex','FontSize', 16)
                    filename = sprintf('../figures/20200715-%03d%s.png', t,obj.prefix);
                    saveas(gcf, filename);
                    m = m + 1;
                end
            end
            %https://www.mathworks.com/matlabcentral/answers/467038-how-to-add-headers-to-excel
            header = {'shape', 'scale'}; %dummy header
            c_theta = cell(size(theta,1)+1, size(theta,2));
            c_theta(1,:) = header;
            c_theta(2:size(theta,1)+1,:) = num2cell(theta);
            filename = sprintf('../data/GammaParam%s.csv',obj.prefix);
            csvwrite(filename, theta);
            
            
        end
        
        
        
        
        %values, confirmed positives for a particular date as days pass by
        %w_0, total confirmed positives the day before
        %mu, sigma, pdf's for reporting in subsequent days
        %w, the kalman filter estimated confirmed positives
        function w = kalman(obj, values, w_0, mu, sigma)
            dt=1;
            %% Initialize state transition matrix
            Psi=[ 1  dt  0;...     % [x  ]
                0  1  dt ;...     % [Vx]
                0  0  1 ];      % [Ax]
            
            Phi = [ 1 0 0];    % Initialize measurement matrix
            
            
            Sigma_p = eye(3); %transition noise covariance
            
            mu_tm1 = [w_0, 0, 0]';  % x_tm1=[x,Vx,Ax]' %state
            Sigma_tm1 = zeros(3, 3);
            
            numPts = length(mu);
            w = zeros(numPts, 1);
            
            figure(1001)
            clf
            plot(values(1:length(mu)),'r','linewidth',1)
            hold on
            alpha = cumsum(mu);
            series = [];
            
            for idx = 1:numPts-1
                z =values(idx)/mu(idx);
                Sigma_m =  (z*sigma(idx)).^2; %measurement noise
                
                %% Predicted state and covariance
                mu_plus = Psi * mu_tm1; %3 x 1
                Sigma_plus = Psi * Sigma_tm1 * Psi' + Sigma_p; %3 x 3
                %% Estimation
                S = Phi * Sigma_plus * Phi' + Sigma_m; % 1 x 1
                B = Sigma_plus * Phi'; % 3 x 1
                K = (S \ B); % (A\B) solves S K = B, K is (3 x 1)
                %% Estimated state and covariance
                mu_t =   mu_plus +   K * (z - Phi * mu_plus); % 3 x 1
                Sigma_t = Sigma_plus - K * Phi * Sigma_plus;
                %% Compute the estimated measurements
                
                series(idx)  =mu_t(1);
                plot(series,'o-b')
                mu_tm1 = mu_t;
                Sigma_tm1 = Sigma_t;
            end                % of the function
            
            hold off
            
            
            mu_plus = Psi * mu_tm1; %3 x 1
            
            Phi = [ mu(numPts) 0 0];    % Initialize measurement matrix
            w(numPts) = Phi*mu_plus;
            
            
            
        end   % of the function
        
        
        %Based on Carlo Tomasi's equation
        %values, confirmed positives for a particular date as days pass by
        %w_0, total confirmed positives the day before
        %mu, sigma, pdf's for reporting in subsequent days
        %w, the kalman filter estimated confirmed positives
        function w = kalman2(obj, values, w_0, mu, in)
            
            q1 = abs(in(1)); q2 = abs(in(2)); q3= abs(in(3));
            r1 = in(4);
            wT = in(5);
            dt=1;
            %% Initialize state transition matrix
            F_k=[ 1  dt  0;...    % [f]
                0  1   0;...    % [f']
                0  0   1];      %  [d]
            
            
            
            Q_k = diag([q1,q2, q3]); %transition noise covariance
            R_k =  r1; %measurement noise
            
            x_k_km1 = [mu(1), 0, w_0/wT]';  % x_tm1=[x,Vx,Ax]' %state
            P_k_km1 = eye(3, 3);
            
            numPts = length(mu);
            
            figure(1001)
            clf
            plot( cumsum(mu),'r','linewidth',1)
            hold on
            alpha = mu;
            series = [];
            
            for idx = 2:numPts-1
                %measurement at time k
                y_k =values(idx)/wT;
                
                %estimates for k given k-1
                d_k = x_k_km1(3);
                f_k = x_k_km1(1);
                
                
                %%%Update
                H_k = [d_k 0 f_k];    % Initialize measurement matrix
                
                P_k_k = inv(inv(P_k_km1) + H_k' * inv(R_k)* H_k); %3 x 3
                
                K_k = P_k_k * H_k' * inv(R_k); % 1 x 1 %gain
                
                x_k_k =   x_k_km1 +   K_k * (y_k - H_k * x_k_km1); % 3 x 1x
                
                
                %% Propagation
                x_kp1_k = F_k * x_k_k; %3 x 1
                P_kp1_k = F_k * P_k_k * F_k' + Q_k; %3 x 3
                
                
                %% Compute the estimated measurements
                
                series(idx)  =x_k_k(1);
                plot(series,'o-b')
                x_k_km1 = x_kp1_k;
                P_k_km1 = P_kp1_k;
            end                % of the function
            
            hold off
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('$f(t)$',  'Interpreter','LaTex','FontSize', 16)
            
            cmu = cumsum(mu);
            error = (series - cmu(1,1:length(series))) * (series - cmu(1,1:length(series)))';
            disp(error)
            
        end   % of the function
        
        
        function [x,fval] = optimizeParameters(obj,values, w_0, mu, x)
            
            options = optimset('MaxFunEvals',100000, 'MaxIter',10000);
            [x,fval] = fminsearch(@kalmanEvaluate,x, options);
            
            
            %Based on Carlo Tomasi's equation
            %values, confirmed positives for a particular date as days pass by
            %w_0, total confirmed positives the day before
            %mu, sigma, pdf's for reporting in subsequent days
            %w, the kalman filter estimated confirmed positives
            function error = kalmanEvaluate(in)
                
                q1 = abs(in(1)); q2 = abs(in(2)); q3= abs(in(3));
                r1 = in(4);
                wT = in(5);
                dt=1;
                %% Initialize state transition matrix
                F_k=[ 1  dt  0;...    % [f]
                    0  1   0;...    % [f']
                    0  0   1];      %  [d]
                
                
                
                Q_k = diag([q1,q2, q3]); %transition noise covariance
                R_k =  r1; %measurement noise
                
                x_k_km1 = [mu(1), 0, w_0/wT]';  % x_tm1=[x,Vx,Ax]' %state
                P_k_km1 = eye(3, 3);
                
                numPts = length(mu);
                
                alpha = mu;
                series = [];
                
                for idx = 2:numPts-1
                    %measurement at time k
                    y_k =values(idx)/wT;
                    
                    %estimates for k given k-1
                    d_k = x_k_km1(3);
                    f_k = x_k_km1(1);
                    
                    
                    %%%Update
                    H_k = [d_k 0 f_k];    % Initialize measurement matrix
                    
                    P_k_k = inv(inv(P_k_km1) + H_k' * inv(R_k)* H_k); %3 x 3
                    
                    K_k = P_k_k * H_k' * inv(R_k); % 1 x 1 %gain
                    
                    x_k_k =   x_k_km1 +   K_k * (y_k - H_k * x_k_km1); % 3 x 1x
                    
                    
                    %% Propagation
                    x_kp1_k = F_k * x_k_k; %3 x 1
                    P_kp1_k = F_k * P_k_k * F_k' + Q_k; %3 x 3
                    
                    
                    %% Compute the estimated measurements
                    
                    series(idx)  =x_k_k(1);
                    
                    x_k_km1 = x_kp1_k;
                    P_k_km1 = P_kp1_k;
                end                % of the function
                cmu = cumsum(mu);
                error = (series - cmu(1,1:length(series))) * (series - cmu(1,1:length(series)))';
                disp(error)
            end   % of the function
            
        end
        
        
        function [q0_025, q0_5, q0_075] = predict(obj, theta, day, before)
            disp(day)
            phat = theta(day,:);
            a = phat(1); b = phat(2);
            r = gamrnd(a,b,[1,1000]);
            y = quantile(before * (1 + r),[0.025  0.50 0.975])
            q0_025 = y(1);
            q0_5 =  y(2);
            q0_075 =  y(3);
        end
        
        
        
        
        function nowcastingCompound(obj, theta)
            [num, txt, raw] = xlsread(obj.masterFile);
            
            %n_rows: number of days for covid
            %n_cols: number of days with public reports
            [n_rows, n_cols] = size(num);
            
            k =45;
            dk = 59;
            %save the observations with about completeness of reports
            
            C = max(num,[],2);
            m = 1;
            for i=k:(n_rows)
                %disp([i,k])
                %disp(t)
                v = num(i, m:end);
                pos = max(find(v == 0));
                c = max(v);
                day = length(v) - pos;
                if day < size(theta,1) & day > 0
                    
                    [minimum,media, maximum] = obj.predict(theta, day, c);
                    Q(m,:) = [i, minimum, media, maximum];
                    m = m + 1;
                end
                
            end
            figure(1)
            clf
            plot(C,'linewidth',2);
            hold on
            tiempo = Q(:,1)';
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
        
        
        
        
        function nowcastingSamples(obj, theta)
            [num, txt, raw] = xlsread(obj.masterFile);
            
            %n_rows: number of days for covid
            %n_cols: number of days with public reports
            [n_rows, n_cols] = size(num);
            n_samples = 1000;
            k =45;
            dk = 59;
            %save the observations with about completeness of reports
            Q = zeros(n_rows, n_samples);
            C = max(num,[],2);
            m = 1;
            for i=1:(n_rows)
                %disp([i,k])
                %disp(t)
                v = num(i, m:end);
                pos = max(find(v == 0));
                if isempty(pos)
                    pos = 0;
                end
                
                c = max(v);
                day = length(v) - pos;
                if day < size(theta,1) & day > 0
                    a = theta(day,1); b = theta(day,2);
                    r = gamrnd(a,b,[1,1000]);
                    Q(i,:) = floor(c * (1 + r));
                    
                else
                    Q(i,:) = C(i);
                end
                
            end
            figure(1)
            clf
            plot(Q,'linewidth',1.5);
            
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('confirmed positives',  'Interpreter','LaTex','FontSize', 16)
            
            
            
            header = {}; %dummy header
            for i=1:n_samples
                num = num2str(i);
                header{i} = num;
            end
            c_Q = cell(size(Q,1)+1, size(Q,2)+1);
            c_Q(1,2:end) = header;
            c_Q(2:end,1) = txt(2:end,1);
            c_Q(2:size(Q,1)+1, 2:size(Q,2)+1) = num2cell(Q);
            axis([0,135,0,3e4])
            filename = sprintf('../data/infectious_samples%s.xlsx', obj.prefix);
            
            
            xlswrite(filename, c_Q)
            
            
            
        end
        
        
        
        
        
        function nowcasting(obj, mu, sigma)
            load('data');
            [rows,cols] = size(V);
            disp(V(rows,end))
            
            %             q1 = in(1); q2 = in(2); q3= in(3);
            %             r1 = in(4);
            %             wT = in(5);
            
            for i=rows:-1:1
                values = V(i-1,:);
                w0 = V(i,end-i);
                %in = [1, 1,1,10,1000];
                in = [0.7271, 0.4262, 0, 0.0049, 5.4189e+03];
                obj.kalman2( values,w0 , mu, in);
                %[x,fval] = obj.optimizeParameters(values, w0, mu, in);
            end
            
        end
        
        
        function saveCSV(obj)
            
            [num, txt, raw] = xlsread(obj.masterFile);
            
            [rows, cols] = size(num);
            
            figure(100)
            mesh(num)
            set(gca, 'FontSize', 16)
            xlabel('days released',  'Interpreter','LaTex','FontSize', 16)
            ylabel('days reported',  'Interpreter','LaTex','FontSize', 16)
            %colormap(gray(256))
            
            
            figure(101)
            [gx, gy] = gradient(num);
            imagesc(gx)
            set(gca, 'FontSize', 16)
            xlabel('difference between days released',  'Interpreter','LaTex','FontSize', 16)
            ylabel('days reported',  'Interpreter','LaTex','FontSize', 16)
            
            
            figure(102)
            ngx = gx./sum(gx,2);
            mesh(ngx)
            axis([1, size(ngx,2), 40, 80, 0, 0.15])
            set(gca, 'FontSize', 16)
            xlabel('difference between days released',  'Interpreter','LaTex','FontSize', 16)
            ylabel('days reported',  'Interpreter','LaTex','FontSize', 16)
            
            
            
            %num = cumsum(num,2);
            figure(1)
            clf
            
            figure(2)
            clf
            
            x = 1:cols;
            k = 19;
            t = 0:-1:-k;
            Y = [];
            X_data = []; %total confirmed positives
            m = 1;
            for i=rows- k + 1:-1:46
                %disp([i,k])
                %disp(t)
                
                y = ngx(rows-k, end-k:end);
                v = num(rows-k, end-k:end);
                c = rand(1,3);
                figure(1)
                hold on
                scatter(-t,y,10, c,'fill')%,'linewidth',2);
                plot(-t,y,'Color',c)%,'linewidth',2);
                hold off
                drawnow;
                
                
                
                figure(2)
                hold on
                %scatter(-t,y,10, c,'fill')%,'linewidth',2);
                plot(-t,cumsum(y),'Color',c)%,'linewidth',2);
                hold off
                drawnow;
                
                
                
                t = [t, -(k+1)];
                k = k + 1;
                Y(m,1:length(y)) =  y;
                X_data(m,1:length(v)) =  v;
                m = m + 1;
            end
            
            %fill up spaces
            [rows, cols] = size(X_data);
            
            Y_data = [];
            for i=1:rows
                value = max(X_data(i,:));
                col_max = find(X_data(i,:) == max(value));
                X_data(i,col_max+1:cols) = value;
                Y_data(i,1:size(X_data,2)) = value;
            end
            
            figure(1)
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('rate of change',  'Interpreter','LaTex','FontSize', 16)
            %axis([1,n,0, max(num(:))])
            axis([0, 60, 0,0.14])
            
            
            figure(2)
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('cumulative',  'Interpreter','LaTex','FontSize', 16)
            %axis([1,n,0, max(num(:))])
            axis([0, 60, 0,1.05])
            
            
            % figure(2)
            % plot(m, 'linewidth',2)
            % set(gca, 'FontSize', 16)
            % xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            % ylabel('mean offset',  'Interpreter','LaTex','FontSize', 16)
            %
            %
            V = Y_data;
            save('data','Y', 'V')
            
            csvwrite('../data/X_data.csv',X_data(:,1:obj.num_max_days));
            csvwrite('../data/Y_data.csv',Y_data(:,1:obj.num_max_days));
            
        end
        
        
        
        %produce synthetic data for training
        function syntheticDataGaussian(obj)
            
            [mu, sigma]  = obj.modelDelaysGaussian(0);
            k =10;
            N = [];
            for f =500:10:5000
                media = mu * f;
                sd = sigma * f;
                
                for numCurves=1:k
                    
                    for timestep=1:length(mu)
                        
                        x = floor(randn(1)*sd(timestep) + media(timestep)+0.5);
                        if x < 0
                            x = 0;
                        end
                        if timestep == 1
                            n(timestep) = x;
                        else
                            n(timestep) = n(timestep-1) + x;
                        end
                        
                    end %timesteps
                    N = [N; n];
                    
                end
                
            end
            
            np = max(N,[], 2);
            M = np * ones(1, size(N,2));
            
            csvwrite('../data/X_syn_data.csv',N);
            csvwrite('../data/Y_syn_data.csv',M);
            
            Z = N./M;
            figure(1)
            
            plot(Z)
            set(gca, 'FontSize', 16)
            xlabel('sample',  'Interpreter','LaTex','FontSize', 16)
            ylabel('positives/total',  'Interpreter','LaTex','FontSize', 16)
            axis([0,size(Z,1), 0,1])
            
        end
        
        
        
        %produce synthetic data for training
        function syntheticDataGamma(obj)
            
            [mu, sigma]  = obj.modelDelaysGamma(0);
            k =10;
            N = [];
            for f =500:10:5000
                media = mu * f;
                sd = sigma * f;
                shape = media.^2./sd.^2;
                scale = sd.^2./media;
                for numCurves=1:k
                    
                    for timestep=1:length(mu)
                        x = floor(gamrnd(shape(timestep), scale(timestep))+0.5);
                        
                        %x = floor(randn(1)*sd(timestep) + media(timestep)+0.5);
                        %if x < 0
                        %    x = 0;
                        %end
                        if timestep == 1
                            n(timestep) = x;
                        else
                            n(timestep) = n(timestep-1) + x;
                        end
                        
                    end %timesteps
                    N = [N; n];
                    
                end
                
            end
            
            np = max(N,[], 2);
            M = np * ones(1, size(N,2));
            
            csvwrite('../data/X_syn_data.csv',N);
            csvwrite('../data/Y_syn_data.csv',M);
            
            Z = N./M;
            figure(1)
            
            plot(Z)
            set(gca, 'FontSize', 16)
            xlabel('sample',  'Interpreter','LaTex','FontSize', 16)
            ylabel('positives/total',  'Interpreter','LaTex','FontSize', 16)
            axis([0,size(Z,1), 0,1])
            
            figure(100)
            plot(N')
            set(gca, 'FontSize', 16)
            xlabel('days',  'Interpreter','LaTex','FontSize', 16)
            ylabel('positives',  'Interpreter','LaTex','FontSize', 16)
            axis([0,size(N,2), 0,6000])
            
            
        end
        
        
        
        %estimates the models for the corresponding state using a set of routines
        %common to all states. Should be easier to debug.
        function    theta = compoundRateOfChange3(obj,masterFile, prefix, draw)
            c = crcCommon;
            theta = c.crc(masterFile, prefix, draw);
        end
        
        
        function nowcastingCompound2(obj, masterFile, prefix, theta, offset)
            c = crcCommon;
            c.nowcasting(masterFile, theta, offset,prefix);
            
        end
        
        function nowcastingSamples2(obj, masterFile, prefix, theta, offset)
            c = crcCommon;
            c.sampling(masterFile, prefix, theta,offset);
            
        end
        
        
        
        function runme(obj)
            % [num,pdfs] = obj.plotDelays();
            %obj.process();
            %[mu, sigma]  = obj.modelDelays();
            %obj.nowcasting(mu, sigma);
            
            %read mat variable and save it to csv
            %obj.saveCSV();
            offset = 5*ones(1,32);
            
            for i=1:length(obj.prefixes)
                
                obj.masterFile = sprintf('../data/updated_delays_%s.csv', obj.prefixes{i});
            
                 
                disp(obj.masterFile)
                theta = obj.compoundRateOfChange3(obj.masterFile, obj.prefixes{i},0);
                if not(isempty(theta)) 
                    disp(i)
                    disp(obj.prefixes{i})
                    obj.nowcastingCompound2(obj.masterFile, obj.prefixes{i}, theta, offset(i));
                    obj.nowcastingSamples2(obj.masterFile, obj.prefixes{i},theta,offset(i));
                end
            end
            
        end
        
        
    end
end

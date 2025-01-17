function [out, varargout] = vocoder(in, fmin, nbch, erbscale, Fs)
% out = vocoder(in, fmin, nbch, erbscale, Fs)
% out is tone vocoded using a gammatone filterbank
% envelope filter cutoff @ 64Hz
% in: In signal
% fmin: Minimum frequency of the filterbank, 1st center frequency
% nbch: Number of channels
% erbscale: Filters central frequencies distribution on ERB scale
%           (1->one filter each ERB; 2->one filter every 2 ERB; ...)
% Fs: Sample frequency
%
% Filterbank parameters using
% [out, Fc, Fb] = vocoder(in, fmin, nbch, erbscale, Fs)
% Fc: Central frequencies of the filterbank
% Fb: Bandwidths of the filterbank


%wider=enlarge(fmin,erbscale); %bandwidth adaptation

[a,fc,fb,deltan,deltamax,phi,mix]=gammabank(fmin,nbch,erbscale,1,Fs); %wider,Fs); %Parameters calculation

Num=2*(1-abs(a)).^4;
for k=1:nbch
    Den(k,:)=poly([a(k) a(k) a(k) a(k)]);
end

x=abs(gammafilter(in,Den,Num,nbch,deltan,deltamax,phi,mix)); %filterbank
t=1:length(x);
for k=1:nbch
    if (fb(k)./2)<64,ff=64*2;
    else ff=fb(k);end
    [Benv,Aenv]=butter(6,ff/Fs); %envelope extraction low pass filter @ ERB/2
    x(k,:)=filter(Benv,Aenv,x(k,:)); %envelope extraction
    %Bn modif: x(k,:)=x(k,:).*sin(2*pi*fc(k).*t./Fs + rand(1)*2*pi); %sinus carrier @ center frequency of the filter
end

out=x; %Bn modif: out=sum(x,1);

varargout(1) = {fc};
varargout(2) = {fb};
varargout(3) = {deltan};




%--------------------------------------------
% function Xfb=enlarge(fmin,F)
% 
% q=9.265;
% l=24.7;
% erbscale=q*log(1+fmin/(l*q));
% q3db=(2*sqrt(2^(1/4)-1))/((720*pi*2^(-6))/36);
% for m=1:2,
%     fc(m)=(exp((erbscale+(m-1)*F)/q)-1)*l*q;
%     erb=l+fc(m)/q;
%     fb(m)=q3db*erb;
% end
% Xfb = 2 * (fc(2) - fc(1))/(fb(1) + fb(2));


%--------------------------------------------
function y=gammafilter(x,den,num,nbch,deltan,deltamax,phi,mix)

%filtering
for n=1:nbch,
    yt(n,:)=filter(num(n),den(n,:),x);
end

%phase alignement and weighting
for n=1:nbch,
    x2(n,:) = cos(phi(n))*real(yt(n,:)) - sin(phi(n))*imag(yt(n,:));
    x3(n,:) = [zeros(1,deltan(n)) x2(n,:) zeros(1,deltamax-deltan(n))];
    y(n,:) = x3(n,:).*mix(n);
end



%--------------------------------------------
function [a,fc,fb,deltan,deltamax,phi,mix]=gammabank(fmin,nbch,ERBpas,fbenlarge,fs)


%%% param?tres d'analyse
q=9.265;
l=24.7;
d=0.016;

erbscale=q*log(1+fmin/(l*q));

q3db=(2*sqrt(2^(1/4)-1))/((720*pi*2^(-6))/36);
q3db=q3db*fbenlarge;

for m=1:nbch,
    fc(m)=(exp((erbscale+(m-1)*ERBpas)/q)-1)*l*q;
    erb=l+fc(m)/q;
    fb(m)=q3db*erb;
    a(m)=acalcul(fc(m),fb(m),fs);
end

%%% param?tres de synth?se
samples=4096;

indmax=floor(d*fs) + 1;
t=0:samples-1;
for n=1:nbch,
    k(n,:)=((a(n).^t)/6).*(t.^3 + 6*t.^2 + 11*t +6).*2*(1-abs(a(n))).^4;
end

[mx,ind]=max(abs(k),[],2);

t=ind'.*(ind'<indmax);
t=t + indmax.*(t==0);
deltan=indmax*ones(1,nbch) - t;
t=t./fs;
phi=-2*pi.*fc.*(t);

deltamax=max(deltan);


for m=1:nbch, 
    ri(m,:)=real(k(m,:).*exp(i*phi(m))); 
    ri(m,:)=[zeros(1,deltan(m)) ri(m,1:samples-deltan(m))];
end

re_sp=fft(real(ri.'));
mix=ones(nbch,1);
si=fc.*samples./fs;
sesp=re_sp(round(si),:);

for m=1:10,
    sesp2=sesp*mix; mix=mix./abs(sesp2); 
end


%-----------------------------
function a=acalcul(fc,fb,fs)

beta=2*pi*fc/fs;
phi=2*pi*(fb/2)/fs;
u=-3/4;
p=(-2+2*10^(u/10)*cos(phi))/(1-10^(u/10));
lambda=-p/2-sqrt((p^2)/4-1);
a=lambda*exp(i*beta);

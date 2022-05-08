FROM plotly/heroku-docker-r:3.6.3_heroku18

# during the build copy the application file to the image
COPY . /app/

# for installing additional dependencies etc.
RUN if [ -f '/app/onbuild' ]; then bash /app/onbuild; fi;

# look for /app/apt-packages and if it exists, install the packages contained
RUN if [ -f '/app/apt-packages' ]; then apt-get update -q && cat apt-packages | xargs apt-get -qy install && rm -rf /var/lib/apt/lists/*; fi;

CMD cd /opt/local/lib

CMD ls libgsl*

CMD cd 

# look for app/init.R and if it exists, execute it
RUN if [ -f '/app/init.R' ]; then /usr/bin/R --no-init-file --no-save --quiet --slave -f /app/init.R; fi;

# here app.R needs to match the name of the file which contains your app
CMD cd /app && /usr/bin/R --no-save -f /app/cryptoRiskAnalysis.R



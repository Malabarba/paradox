FROM silex/emacs:26.2-alpine-dev
# ARG RACK_ENV
# ARG PORT
ENV APP_HOME /app
# RUN apt-get update -qq && apt-get install -y build-essential
RUN mkdir $APP_HOME
WORKDIR $APP_HOME
ADD . $APP_HOME/
RUN cask install || cask install
# RUN bundle install — without development test
CMD /bin/sh -c cask exec ert-runner

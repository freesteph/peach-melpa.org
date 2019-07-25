FROM peach-melpa:latest

ENV DISPLAY=":13"

RUN apk add emacs-x11 xvfb imagemagick curl

RUN chmod +x ./install-font.sh
RUN ./install-font.sh

COPY wrapper.sh .
RUN chmod +x ./wrapper.sh

ENTRYPOINT ["/bin/sh", "./wrapper.sh"]

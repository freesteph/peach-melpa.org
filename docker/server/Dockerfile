FROM peach-melpa:latest

EXPOSE 80 443 3000

COPY ./wrapper.sh .
RUN chmod +x ./wrapper.sh

ENTRYPOINT ./wrapper.sh

CMD ["bundle", "exec", "rails", "server"]

FROM rabbitmq:3.7-management
MAINTAINER  Ilya Khaprov <i.khaprov@gmail.com>

# prometheus exporter plugin
ADD ["plugins/accept-*", \
     "plugins/prometheus-*", \
     "plugins/prometheus_httpd-*",\
     "plugins/prometheus_cowboy-*", \
     "plugins/prometheus_process_collector-*", \
     "plugins/prometheus_rabbitmq_exporter-*", \
     "/usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/"]
RUN chmod a+r /usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/prometheus*.ez /usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/accept*.ez \
    && rabbitmq-plugins enable --offline prometheus accept prometheus_rabbitmq_exporter prometheus_process_collector prometheus_httpd prometheus_cowboy \
    && chmod -R 777 /etc/rabbitmq

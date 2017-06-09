FROM rabbitmq:3.6.10
MAINTAINER  Ilya Khaprov <i.khaprov@gmail.com>

# rabbitmq-management
RUN rabbitmq-plugins enable --offline rabbitmq_management
EXPOSE 15671 15672

# prometheus exporter plugin
ADD ["https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.9.1/accept-0.3.0.ez", \
     "https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.9.1/prometheus-3.2.2.ez", \
     "https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.9.1/prometheus_httpd-1.1.1.ez", \
     "https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.9.1/prometheus_process_collector-1.0.2.ez", \
     "https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.9.1/prometheus_rabbitmq_exporter-v3.6.9.1.ez", \
     "/usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/"]
RUN chmod a+r /usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/prometheus*.ez /usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/accept*.ez \
    && rabbitmq-plugins enable --offline prometheus accept prometheus_rabbitmq_exporter prometheus_process_collector prometheus_httpd \
    && chmod -R 777 /etc/rabbitmq

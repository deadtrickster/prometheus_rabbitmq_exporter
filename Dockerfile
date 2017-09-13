FROM rabbitmq:3.6.12
MAINTAINER  Ilya Khaprov <i.khaprov@gmail.com>

# rabbitmq-management
RUN rabbitmq-plugins enable --offline rabbitmq_management
EXPOSE 15671 15672

# prometheus exporter plugin
ADD ["https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.12.1/accept-0.3.3.ez", \
     "https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.12.1/prometheus-3.4.0.ez", \
     "https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.12.1/prometheus_httpd-2.1.4.ez", \
     "https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.12.1/prometheus_process_collector-1.1.0.ez", \
     "https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/download/rabbitmq-3.6.12.1/prometheus_rabbitmq_exporter-v3.6.12.1.ez", \
     "/usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/"]
RUN chmod a+r /usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/prometheus*.ez /usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/accept*.ez \
    && rabbitmq-plugins enable --offline prometheus accept prometheus_rabbitmq_exporter prometheus_process_collector prometheus_httpd \
    && chmod -R 777 /etc/rabbitmq

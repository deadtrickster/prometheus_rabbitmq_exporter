FROM rabbitmq:3.7-management
MAINTAINER  Ilya Khaprov <i.khaprov@gmail.com>

# Copy the plugin and its dependencies
COPY "plugins/*.ez" "/usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/"
RUN chmod a+r /usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/prometheus*.ez /usr/lib/rabbitmq/lib/rabbitmq_server-${RABBITMQ_VERSION}/plugins/accept*.ez \
    && rabbitmq-plugins enable --offline prometheus_rabbitmq_exporter

ARG FROM_IMAGE
ARG PROMETHEUS_RABBITMQ_EXPORTER_VERSION
FROM ${FROM_IMAGE}
MAINTAINER  Ilya Khaprov <i.khaprov@gmail.com>

COPY tmp/*.ez /plugins/
RUN chmod a+r /plugins/*.ez && \
    rabbitmq-plugins enable --offline prometheus_rabbitmq_exporter && \
    rabbitmq-plugins is_enabled prometheus_rabbitmq_exporter --offline && \
    rabbitmq-plugins list | grep "prometheus_rabbitmq_exporter.*${PROMETHEUS_RABBITMQ_EXPORTER_VERSION}"

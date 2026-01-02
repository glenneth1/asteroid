#!/bin/sh

# Generate icecast.xml from base config
# - Substitute hostname (defaults to localhost for dev)
# - If ICECAST_ENABLE_YP=true, insert YP directory blocks

cp /etc/icecast-base.xml /etc/icecast.xml

# Set hostname (defaults to localhost if not specified)
ICECAST_HOSTNAME=${ICECAST_HOSTNAME:-localhost}
echo "Icecast hostname: $ICECAST_HOSTNAME"
sed -i "s|<hostname>localhost</hostname>|<hostname>$ICECAST_HOSTNAME</hostname>|" /etc/icecast.xml

if [ "$ICECAST_ENABLE_YP" = "true" ]; then
    echo "YP directory publishing ENABLED"
    # Insert YP config before closing </icecast> tag
    # Use sed with a temp file to handle multi-line insertion
    head -n -1 /etc/icecast.xml > /tmp/icecast-temp.xml
    cat /etc/icecast-yp-snippet.xml >> /tmp/icecast-temp.xml
    echo "</icecast>" >> /tmp/icecast-temp.xml
    mv /tmp/icecast-temp.xml /etc/icecast.xml
else
    echo "YP directory publishing DISABLED (dev mode)"
fi

# Start icecast
exec icecast -c /etc/icecast.xml

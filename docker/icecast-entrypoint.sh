#!/bin/sh

# Generate icecast.xml from base config
# - Substitute hostname (defaults to localhost for dev)
# - If ICECAST_ENABLE_YP=true, insert YP directory blocks

cp /etc/icecast.xml.base /etc/icecast.xml

# Set hostname (defaults to localhost if not specified)
ICECAST_HOSTNAME=${ICECAST_HOSTNAME:-localhost}
echo "Icecast hostname: $ICECAST_HOSTNAME"
sed -i "s|<hostname>localhost</hostname>|<hostname>$ICECAST_HOSTNAME</hostname>|" /etc/icecast.xml

if [ "$ICECAST_ENABLE_YP" = "true" ]; then
    echo "YP directory publishing ENABLED"
    # Insert YP config before closing </icecast> tag
    sed -i 's|</icecast>|'"$(cat /etc/icecast-yp.xml.snippet)"'\n</icecast>|' /etc/icecast.xml
else
    echo "YP directory publishing DISABLED (dev mode)"
fi

# Start icecast
exec icecast -c /etc/icecast.xml

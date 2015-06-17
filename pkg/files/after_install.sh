#!/bin/bash
chown -R pine.pine /opt/pine

cat > /etc/init.d/pine <<EOF
#!/bin/sh
### BEGIN INIT INFO
# Provides:
# Required-Start:    $remote_fs
# Required-Stop:     $remote_fs
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: PINEngine
# Description:       PIN generation and lifecycle Engine
### END INIT INFO

case "$1" in
  start)
    echo "Starting pine..."
    sudo -u pine /opt/pine/bin/pine start
    ;;
  stop)
    echo "Stopping pine..."
    sudo -u pine /opt/pine/bin/pine stop
    ;;
  restart)
    echo "Restarting pine..."
    sudo -u pine /opt/pine/bin/pine restart
    ;;
  status)
    echo "Checking pine..."
    echo -u pine /opt/pine/bin/pine ping
    ;;
  *)
    echo "Usage: $0 {start|stop|restart|status}"
    exit 1
    ;;
esac
EOF
chmod +x /etc/init.d/pine

while true; do
	capacity=$(cat /sys/class/power_supply/BAT0/capacity)

    if [ $capacity -le 16 ]; then
        termite -e "sleep 100"
    fi

    sleep 256
done

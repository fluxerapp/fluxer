trap 'exit 0' TERM INT
buckets="$FLUXER_S3_BUCKET_CDN $FLUXER_S3_BUCKET_UPLOADS $FLUXER_S3_BUCKET_REPORTS $FLUXER_S3_BUCKET_HARVESTS"
missing="$buckets"
for attempt in $(seq 1 "$FLUXER_SEAWEEDFS_INIT_ATTEMPTS"); do
  if ! nc -z 127.0.0.1 9333 2>/dev/null; then
    sleep 2
    continue
  fi
  listed=$(echo "s3.bucket.list" | timeout 10 weed shell -master=127.0.0.1:9333 2>&1)
  missing=""
  for b in $buckets; do
    echo "$listed" | grep -q "^[[:space:]]*$b[[:space:]]" || missing="${missing:+$missing }$b"
  done
  if [ -z "$missing" ]; then
    if ! echo "s3.configure -user=fluxer -access_key=$FLUXER_S3_ACCESS_KEY -secret_key=$FLUXER_S3_SECRET_KEY -actions=Admin,Read,Write,List,Tagging -apply" | timeout 10 weed shell -master=127.0.0.1:9333 >/dev/null 2>&1; then
      echo "seaweedfs-init could not configure the S3 identity" >&2
      exit 1
    fi
    echo "buckets ready"
    touch /tmp/fluxer-buckets-ready
    while :; do
      sleep 86400 &
      wait $!
    done
  fi
  for b in $missing; do
    echo "s3.bucket.create -name $b" | timeout 10 weed shell -master=127.0.0.1:9333 >/dev/null 2>&1
  done
  sleep 2
done
echo "seaweedfs-init could not verify buckets: $missing" >&2
exit 1

trap 'exit 0' TERM INT
buckets="$FLUXER_S3_BUCKET_CDN $FLUXER_S3_BUCKET_UPLOADS $FLUXER_S3_BUCKET_REPORTS $FLUXER_S3_BUCKET_HARVESTS"
missing="$buckets"
for _attempt in $(seq 1 "$FLUXER_SEAWEEDFS_INIT_ATTEMPTS"); do
  if ! nc -z 127.0.0.1 9333 2>/dev/null; then
    sleep 2
    continue
  fi
  listed=$(echo "s3.bucket.list" | timeout 10 weed shell -master=127.0.0.1:9333 2>&1)
  missing=""
  failure=""
  for b in $buckets; do
    echo "$listed" | grep -q "^[[:space:]]*${b}[[:space:]]" || missing="${missing:+$missing }$b"
  done
  if [ -z "$missing" ]; then
    if [ "$FLUXER_SEAWEEDFS_UPLOADS_TTL" = off ]; then
      ttl_rule="fs.configure -locationPrefix=/buckets/$FLUXER_S3_BUCKET_UPLOADS/ -delete -apply"
    else
      ttl_rule="fs.configure -locationPrefix=/buckets/$FLUXER_S3_BUCKET_UPLOADS/ -ttl=$FLUXER_SEAWEEDFS_UPLOADS_TTL -apply"
    fi
    failure="could not configure the S3 identity"
    if echo "s3.configure -user=fluxer -access_key=$FLUXER_S3_ACCESS_KEY -secret_key=$FLUXER_S3_SECRET_KEY -actions=Admin,Read,Write,List,Tagging -apply" | timeout 10 weed shell -master=127.0.0.1:9333 >/dev/null 2>&1; then
      failure="could not set the expiry of $FLUXER_S3_BUCKET_UPLOADS"
      if echo "$ttl_rule" | timeout 10 weed shell -master=127.0.0.1:9333 >/dev/null 2>&1; then
        echo "buckets ready"
        touch /tmp/fluxer-buckets-ready
        while :; do
          sleep 86400 &
          wait $!
        done
      fi
    fi
    sleep 2
    continue
  fi
  for b in $missing; do
    echo "s3.bucket.create -name $b" | timeout 10 weed shell -master=127.0.0.1:9333 >/dev/null 2>&1
  done
  sleep 2
done
if [ -n "$failure" ]; then
  echo "seaweedfs-init $failure" >&2
else
  echo "seaweedfs-init could not verify buckets: $missing" >&2
fi
exit 1

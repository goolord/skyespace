for i in *; do
  cwebp -m 6 -exact $i -o ${i%.png}.webp
done

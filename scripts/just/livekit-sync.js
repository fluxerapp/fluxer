#!/usr/bin/env node
const fs = require('node:fs');
const path = require('node:path');

const args = process.argv.slice(2);
const outputIdx = args.indexOf('--output');
const outputArg = outputIdx >= 0 && args[outputIdx + 1] ? args[outputIdx + 1] : 'dev/livekit.yaml';

const voiceEnabled = (process.env.VOICE_ENABLED || '').trim().toLowerCase() === 'true';
if (!voiceEnabled) {
	process.exit(0);
}

const apiKey = (process.env.LIVEKIT_API_KEY || '').trim();
const apiSecret = (process.env.LIVEKIT_API_SECRET || '').trim();
const webhookUrl = (process.env.LIVEKIT_WEBHOOK_URL || '').trim();
if (!apiKey || !apiSecret || !webhookUrl) {
	process.exit(0);
}

const redisUrl = (process.env.REDIS_URL || '').trim();
const redisAddr = redisUrl.replace(/^redis:\/\//, '') || 'redis:6379';

const yaml = `port: 7880

redis:
  address: "${redisAddr}"
  db: 0

keys:
  "${apiKey}": "${apiSecret}"

rtc:
  tcp_port: 7881

webhook:
  api_key: "${apiKey}"
  urls:
    - "${webhookUrl}"

room:
  auto_create: true
  max_participants: 100
  empty_timeout: 300

development: true

`;
const outputPath = path.resolve(outputArg);
fs.mkdirSync(path.dirname(outputPath), {recursive: true});
fs.writeFileSync(outputPath, yaml, {encoding: 'utf-8', mode: 0o600});

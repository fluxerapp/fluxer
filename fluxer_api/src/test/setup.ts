/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

process.env.NODE_ENV = 'development';
process.env.PORT = '3000';

process.env.DATABASE_URL = 'postgresql://test:test@localhost:5432/test';

process.env.CASSANDRA_HOSTS = 'localhost';
process.env.CASSANDRA_KEYSPACE = 'fluxer';
process.env.CASSANDRA_LOCAL_DC = 'datacenter1';

process.env.REDIS_URL = 'redis://localhost:6379';

process.env.FLUXER_GATEWAY_RPC_HOST = 'localhost';
process.env.FLUXER_GATEWAY_RPC_PORT = '9082';
process.env.GATEWAY_RPC_SECRET = 'test-rpc-secret';

process.env.FLUXER_API_PUBLIC_ENDPOINT = 'https://api.test';
process.env.FLUXER_API_CLIENT_ENDPOINT = 'https://api-client.test';
process.env.FLUXER_APP_ENDPOINT = 'https://app.test';
process.env.FLUXER_GATEWAY_ENDPOINT = 'https://gateway.test';
process.env.FLUXER_MEDIA_ENDPOINT = 'https://media.test';
process.env.FLUXER_CDN_ENDPOINT = 'https://cdn.test';
process.env.FLUXER_MARKETING_ENDPOINT = 'https://marketing.test';
process.env.FLUXER_PATH_MARKETING = '/marketing';
process.env.FLUXER_ADMIN_ENDPOINT = 'https://admin.test';
process.env.FLUXER_PATH_ADMIN = '/admin';
process.env.FLUXER_INVITE_ENDPOINT = 'https://invite.test';
process.env.FLUXER_GIFT_ENDPOINT = 'https://gift.test';
process.env.FLUXER_UNFURL_IGNORED_HOSTS = '';

process.env.MEDIA_PROXY_HOST = 'localhost:8082';
process.env.MEDIA_PROXY_ENDPOINT = 'http://localhost:8082';
process.env.MEDIA_PROXY_SECRET_KEY = 'test-media-secret';

process.env.AWS_ACCESS_KEY_ID = 'test-access-key';
process.env.AWS_SECRET_ACCESS_KEY = 'test-secret-key';
process.env.AWS_S3_ENDPOINT = 'http://localhost:9000';
process.env.AWS_S3_BUCKET_CDN = 'test-cdn';
process.env.AWS_S3_BUCKET_UPLOADS = 'test-uploads';
process.env.AWS_S3_BUCKET_REPORTS = 'test-reports';
process.env.AWS_S3_BUCKET_HARVESTS = 'test-harvests';
process.env.AWS_S3_BUCKET_DOWNLOADS = 'test-downloads';

process.env.CASSANDRA_USERNAME = 'test-cassandra-user';
process.env.CASSANDRA_PASSWORD = 'test-cassandra-pass';

process.env.EMAIL_ENABLED = 'false';
process.env.SMS_ENABLED = 'false';
process.env.CAPTCHA_ENABLED = 'false';
process.env.VOICE_ENABLED = 'false';
process.env.SEARCH_ENABLED = 'false';
process.env.STRIPE_ENABLED = 'false';
process.env.CLOUDFLARE_PURGE_ENABLED = 'false';
process.env.CLAMAV_ENABLED = 'false';

process.env.FLUXER_APP_HOST = 'localhost:3000';
process.env.FLUXER_APP_PROTOCOL = 'http';

process.env.GEOIP_HOST = 'geoip.test';
process.env.GEOIP_PROVIDER = 'ipinfo';
process.env.SUDO_MODE_SECRET = 'test-sudo-secret';

import {vi} from 'vitest';

vi.mock('~/Logger', () => ({
	Logger: {
		info: vi.fn(),
		warn: vi.fn(),
		error: vi.fn(),
		debug: vi.fn(),
		trace: vi.fn(),
		fatal: vi.fn(),
		child: vi.fn(() => ({
			info: vi.fn(),
			warn: vi.fn(),
			error: vi.fn(),
			debug: vi.fn(),
			trace: vi.fn(),
			fatal: vi.fn(),
		})),
	},
}));

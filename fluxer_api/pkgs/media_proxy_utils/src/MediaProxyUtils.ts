// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {buildExternalMediaProxyPath} from '@pkgs/media_proxy_utils/src/ExternalMediaProxyPathCodec';

export interface ExternalMediaProxyURLOptions {
	inputURL: string;
	mediaProxyEndpoint: string;
	mediaProxySecretKey: string;
}

export function getExternalMediaProxyURL(options: ExternalMediaProxyURLOptions): string {
	const endpoint = options.mediaProxyEndpoint.replace(/\/+$/u, '');
	const proxyUrlPath = buildExternalMediaProxyPath(options.inputURL);
	const signature = crypto
		.createHmac('sha256', options.mediaProxySecretKey)
		.update(proxyUrlPath)
		.digest('base64url')
		.replace(/=*$/, '');
	return `${endpoint}/external/${signature}/${proxyUrlPath}`;
}

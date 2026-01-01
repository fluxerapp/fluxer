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

import crypto from 'node:crypto';

const BASE64_URL_REGEX = /=*$/;

const encodeComponent = (component: string) => encodeURIComponent(component).replace(/%2F/g, '/');
const decodeComponent = (component: string) => decodeURIComponent(component);

export const getProxyURLPath = (inputUrl: string): string => {
	const parsedUrl = new URL(inputUrl);
	const query = parsedUrl.search.slice(1);
	const protocol = parsedUrl.protocol.slice(0, -1);
	const hostname = parsedUrl.hostname;
	const port = parsedUrl.port;
	const path = parsedUrl.pathname.slice(1);
	const encodedQuery = query ? `${encodeComponent(query)}/` : '';
	const encodedHostname = encodeComponent(hostname);
	const encodedPort = port ? `:${encodeComponent(port)}` : '';
	const encodedPath = encodeComponent(path);
	return `${encodedQuery}${protocol}/${encodedHostname}${encodedPort}/${encodedPath}`;
};

export const createSignature = (inputString: string, mediaProxySecretKey: string): string => {
	const hmac = crypto.createHmac('sha256', mediaProxySecretKey);
	hmac.update(inputString);
	return hmac.digest('base64url').replace(BASE64_URL_REGEX, '');
};

interface MediaProxyParams {
	inputURL: string;
	mediaProxyEndpoint: string;
	mediaProxySecretKey: string;
}

export const getExternalMediaProxyURL = ({
	inputURL,
	mediaProxyEndpoint,
	mediaProxySecretKey,
}: MediaProxyParams): string => {
	const proxyUrlPath = getProxyURLPath(inputURL);
	const proxyUrlSignature = createSignature(proxyUrlPath, mediaProxySecretKey);
	return `${mediaProxyEndpoint}/external/${proxyUrlSignature}/${proxyUrlPath}`;
};

export const verifySignature = (
	proxyUrlPath: string,
	providedSignature: string,
	mediaProxySecretKey: string,
): boolean => {
	const expectedSignature = createSignature(proxyUrlPath, mediaProxySecretKey);
	return crypto.timingSafeEqual(Buffer.from(expectedSignature), Buffer.from(providedSignature));
};

export const reconstructOriginalURL = (proxyUrlPath: string): string => {
	const parts = proxyUrlPath.split('/');
	let currentIndex = 0;
	let query = '';
	if (parts[currentIndex].includes('%3D')) {
		query = decodeComponent(parts[currentIndex]);
		currentIndex += 1;
	}
	const protocol = parts[currentIndex++];
	if (!protocol) throw new Error('Protocol is missing in the proxy URL path.');
	const hostPart = parts[currentIndex++];
	if (!hostPart) throw new Error('Hostname is missing in the proxy URL path.');
	const [encodedHostname, encodedPort] = hostPart.split(':');
	const hostname = decodeComponent(encodedHostname);
	const port = encodedPort ? decodeComponent(encodedPort) : '';
	const encodedPath = parts.slice(currentIndex).join('/');
	const path = decodeComponent(encodedPath);
	return `${protocol}://${hostname}${port ? `:${port}` : ''}/${path}${query ? `?${query}` : ''}`;
};

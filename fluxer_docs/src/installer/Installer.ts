// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import powershellInstallerSource from '@/installer/install.ps1?raw';
import shellInstallerSource from '@/installer/install.sh?raw';

interface InstallerScript {
	readonly source: string;
	readonly digest: string;
	readonly checksumLine: string;
}

function digestOf(text: string): string {
	return createHash('sha256').update(new TextEncoder().encode(text)).digest('hex');
}

function installerScript(name: string, source: string): InstallerScript {
	const digest = digestOf(source);
	return {source, digest, checksumLine: `${digest}  ${name}\n`};
}

function plainText(body: string): Response {
	const bytes = new TextEncoder().encode(body);
	return new Response(bytes, {
		headers: {
			'cache-control': 'no-store',
			'content-length': bytes.byteLength.toString(),
			'content-type': 'text/plain; charset=utf-8',
			etag: `"${digestOf(body)}"`,
		},
	});
}

export function installerScriptResponse(script: InstallerScript): Response {
	return plainText(script.source);
}

export function installerChecksumResponse(script: InstallerScript): Response {
	return plainText(script.checksumLine);
}

export const shellInstaller = installerScript('install.sh', shellInstallerSource);

export const powershellInstaller = installerScript('install.ps1', powershellInstallerSource);

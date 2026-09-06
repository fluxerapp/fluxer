// SPDX-License-Identifier: AGPL-3.0-or-later

import {installerChecksumLine, installerDigest} from './InstallerDigest';
import powershellInstallerSource from './install.ps1?raw';
import shellInstallerSource from './install.sh?raw';

const CONTENT_TYPE = 'text/plain; charset=utf-8';
const CACHE_CONTROL = 'no-store';

interface InstallerScript {
	readonly name: string;
	readonly source: string;
	readonly digest: string;
	readonly checksumLine: string;
}

function plainTextResponse(body: string, etag: string): Response {
	const bytes = new TextEncoder().encode(body);
	return new Response(bytes, {
		headers: {
			'cache-control': CACHE_CONTROL,
			'content-length': bytes.byteLength.toString(),
			'content-type': CONTENT_TYPE,
			etag: `"${etag}"`,
		},
	});
}

function installerScript(name: string, source: string): InstallerScript {
	return {name, source, digest: installerDigest(source), checksumLine: installerChecksumLine(name, source)};
}

export function installerScriptResponse(script: InstallerScript): Response {
	return plainTextResponse(script.source, script.digest);
}

export function installerChecksumResponse(script: InstallerScript): Response {
	return plainTextResponse(script.checksumLine, installerDigest(script.checksumLine));
}

export const shellInstaller = installerScript('install.sh', shellInstallerSource);

export const powershellInstaller = installerScript('install.ps1', powershellInstallerSource);

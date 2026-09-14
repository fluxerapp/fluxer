// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';

const INSTALLER_DIGEST_ALGORITHM = 'sha256';

export function installerDigest(source: string): string {
	return createHash(INSTALLER_DIGEST_ALGORITHM).update(new TextEncoder().encode(source)).digest('hex');
}

export function installerChecksumLine(name: string, source: string): string {
	return `${installerDigest(source)}  ${name}\n`;
}

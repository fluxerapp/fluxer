// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import type {VirusScanResult} from '@pkgs/virus_scan/src/VirusScanResult';

export class DisabledVirusScanService implements IVirusScanService {
	readonly enabled = false;
	async scanFile(filePath: string): Promise<VirusScanResult> {
		const fileHash = crypto.createHash('sha256').update(filePath).digest('hex');
		return {
			isClean: true,
			fileHash,
		};
	}
}

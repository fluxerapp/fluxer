// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VirusScanResult} from '@pkgs/virus_scan/src/VirusScanResult';

export interface IVirusScanService {
	readonly enabled: boolean;
	scanFile(filePath: string): Promise<VirusScanResult>;
}

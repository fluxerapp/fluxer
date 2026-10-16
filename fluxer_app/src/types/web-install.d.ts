// SPDX-License-Identifier: AGPL-3.0-or-later

export interface WebInstallResult {
	manifestId: string;
}

declare global {
	interface Navigator {
		install?: (installUrl?: string, manifestId?: string) => Promise<WebInstallResult>;
	}
}

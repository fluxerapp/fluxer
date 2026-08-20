// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MasterConfig} from '@fluxer/config/src/MasterConfig';

export interface S3ProviderSettings {
	endpoint: string;
	presignedUrlBase?: string;
	forcePathStyle: boolean;
	region: string;
	accessKeyId: string;
	secretAccessKey: string;
}

export interface ResolvedDownloadsProvider {
	settings: S3ProviderSettings;
	isOverridden: boolean;
}

export function resolveDownloadsProvider(master: Pick<MasterConfig, 's3' | 's3_downloads'>): ResolvedDownloadsProvider {
	const base = master.s3;
	if (!base) {
		throw new Error('S3 configuration is required to resolve the downloads provider');
	}
	const baseSettings: S3ProviderSettings = {
		endpoint: base.endpoint,
		presignedUrlBase: base.presigned_url_base,
		forcePathStyle: base.force_path_style,
		region: base.region,
		accessKeyId: base.access_key_id,
		secretAccessKey: base.secret_access_key,
	};
	const override = master.s3_downloads;
	if (!override?.endpoint) {
		return {settings: baseSettings, isOverridden: false};
	}
	return {
		settings: {
			endpoint: override.endpoint,
			presignedUrlBase: override.presigned_url_base ?? undefined,
			forcePathStyle: override.force_path_style ?? baseSettings.forcePathStyle,
			region: override.region ?? baseSettings.region,
			accessKeyId: override.access_key_id ?? baseSettings.accessKeyId,
			secretAccessKey: override.secret_access_key ?? baseSettings.secretAccessKey,
		},
		isOverridden: true,
	};
}

// SPDX-License-Identifier: AGPL-3.0-or-later

export interface S3ProviderSettings {
	endpoint: string;
	presignedUrlBase?: string;
	forcePathStyle: boolean;
	region: string;
	accessKeyId: string;
	secretAccessKey: string;
}

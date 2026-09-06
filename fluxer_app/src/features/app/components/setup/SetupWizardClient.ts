// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import type {
	BrandingAssetUploadRequest,
	InstanceConfigResponse,
	InstanceConfigUpdateRequest,
	InstanceEmailSmtpTestRequest,
	InstanceEmailSmtpTestResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';

export type SetupBrandingAssetKind = BrandingAssetUploadRequest['kind'];

export async function fetchInstanceConfig(): Promise<InstanceConfigResponse> {
	const response = await http.get<InstanceConfigResponse>(Endpoints.ADMIN_INSTANCE_CONFIG);
	return response.body;
}

export async function updateInstanceConfig(body: InstanceConfigUpdateRequest): Promise<InstanceConfigResponse> {
	const response = await http.patch<InstanceConfigResponse>(Endpoints.ADMIN_INSTANCE_CONFIG, {body});
	return response.body;
}

export async function uploadBrandingAsset(
	kind: SetupBrandingAssetKind,
	image: string | null,
): Promise<InstanceConfigResponse> {
	const body: BrandingAssetUploadRequest = {kind, image};
	const response = await http.post<InstanceConfigResponse>(Endpoints.ADMIN_INSTANCE_CONFIG_BRANDING_ASSETS, {body});
	return response.body;
}

export async function testSmtpConfig(body: InstanceEmailSmtpTestRequest): Promise<InstanceEmailSmtpTestResponse> {
	const response = await http.post<InstanceEmailSmtpTestResponse>(Endpoints.ADMIN_INSTANCE_CONFIG_SMTP_TESTS, {body});
	return response.body;
}

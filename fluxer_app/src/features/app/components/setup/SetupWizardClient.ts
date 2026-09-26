// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import SessionManager from '@app/features/platform/state/AuthSession';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	BrandingAssetUploadRequest,
	InstanceConfigResponse,
	InstanceConfigUpdateRequest,
	InstanceEmailSmtpTestRequest,
	InstanceEmailSmtpTestResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';

const logger = new Logger('SetupWizardClient');

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

export type SetupUnauthorizedCause = 'stale_session' | 'origin_mismatch' | 'unknown';

export async function classifySetupUnauthorized(): Promise<SetupUnauthorizedCause> {
	if (!SessionManager.token) return 'unknown';
	if (!http.carriesAuthorization()) return 'origin_mismatch';
	try {
		const response = await http.get(Endpoints.USER_ME, {mode: 'silent'});
		return response.status === 401 ? 'stale_session' : 'unknown';
	} catch (error) {
		logger.warn('Could not confirm whether the setup session is still valid', error);
		return 'unknown';
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	InstanceAppPublic,
	InstanceCaptcha,
	InstanceCommunity,
	InstanceDiscoveryResponse,
	InstanceEndpoints,
	InstanceFeatures,
	InstanceGif,
	InstancePush,
	InstanceRegistration,
	InstanceServices,
	InstanceSso,
} from '@fluxer/instance_bootstrap/src/Types';
import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';

export interface DiscoveryStaticInput {
	apiCodeVersion: number;
	endpoints: InstanceEndpoints;
	captcha: InstanceCaptcha;
	features: InstanceFeatures;
	gif: InstanceGif;
	push: InstancePush;
	appPublic: InstanceAppPublic;
}

export interface DiscoveryDynamicInput {
	sso: InstanceSso;
	registration: InstanceRegistration;
	community: InstanceCommunity;
	services: InstanceServices;
	limits: LimitConfigSnapshot | LimitConfigWireFormat;
}

export function buildDiscoveryResponse(
	staticInput: DiscoveryStaticInput,
	dynamicInput: DiscoveryDynamicInput,
): InstanceDiscoveryResponse {
	return {
		api_code_version: staticInput.apiCodeVersion,
		endpoints: staticInput.endpoints,
		captcha: staticInput.captcha,
		features: staticInput.features,
		gif: staticInput.gif,
		sso: dynamicInput.sso,
		registration: dynamicInput.registration,
		community: dynamicInput.community,
		services: dynamicInput.services,
		limits: dynamicInput.limits,
		push: staticInput.push,
		app_public: staticInput.appPublic,
	};
}

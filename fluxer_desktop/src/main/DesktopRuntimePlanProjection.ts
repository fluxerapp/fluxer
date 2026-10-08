// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildLocalAppRuntimeURL, type LocalAppRuntimePlan} from '@electron/main/LocalAppRuntimePlans';
import {LOCAL_APP_API_PATH_PREFIX} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';
import type {DesktopRuntimePlan} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';

export function projectDesktopRuntimePlan(plan: LocalAppRuntimePlan): DesktopRuntimePlan {
	return {
		instanceKey: plan.instanceKey,
		apiEndpoint: buildLocalAppRuntimeURL(LOCAL_APP_API_PATH_PREFIX, plan.instanceKey),
		remoteApiEndpoint: plan.endpoints.apiEndpoint,
		document: plan.document,
	};
}

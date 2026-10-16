// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getFluxerDebugObject} from '@app/features/platform/utils/FluxerDebugGlobal';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';

const logger = new Logger('VoiceSubscriptionDebugApi');

async function collectReport(): Promise<unknown> {
	const {collectVoiceSubscriptionDebugReport} = await loadLazyModule(
		() => import('@app/features/voice/diagnostics/VoiceSubscriptionDebugReport'),
	);
	return collectVoiceSubscriptionDebugReport();
}

export function installVoiceSubscriptionDebugApi(): void {
	const debugObject = getFluxerDebugObject();
	if (!debugObject) {
		return;
	}
	try {
		debugObject.getVoiceSubscriptionDebug = collectReport;
		debugObject.getVoiceSubscriptionDebugJson = async () => JSON.stringify(await collectReport(), null, 2);
	} catch (error) {
		logger.warn('Failed to install __FLUXER_DEBUG__ voice subscription helpers', error);
	}
}

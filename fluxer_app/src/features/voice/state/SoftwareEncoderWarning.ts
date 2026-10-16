// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('SoftwareEncoderWarning');
const NEVER_SHOW_AGAIN_KEY = 'SoftwareEncoderWarning_neverShowAgain';

interface EncoderInfo {
	codec: string | null;
	implementation: string;
	source: 'encoder' | 'decoder';
}

class SoftwareEncoderWarning {
	showWarning = false;
	encoderInfo: EncoderInfo | null = null;
	neverShowAgain: boolean;

	constructor() {
		this.neverShowAgain = AppStorage.getItem(NEVER_SHOW_AGAIN_KEY) === 'true';
		makeAutoObservable(this, undefined, {autoBind: true});
	}

	triggerWarning(codec: string, implementation: string): void {
		if (this.neverShowAgain) {
			logger.debug('Software encoder warning suppressed by user preference', {codec, implementation});
			return;
		}
		this.encoderInfo = {codec, implementation, source: 'encoder'};
		this.showWarning = true;
		logger.info('Software encoder warning triggered', {codec, implementation});
	}

	triggerDecoderWarning(codec: string | null, implementation: string): void {
		if (this.neverShowAgain) {
			logger.debug('Software decoder warning suppressed by user preference', {codec, implementation});
			return;
		}
		this.encoderInfo = {codec, implementation, source: 'decoder'};
		this.showWarning = true;
		logger.info('Software decoder warning triggered', {codec, implementation});
	}

	dismiss(): void {
		this.showWarning = false;
	}

	dismissForever(): void {
		this.showWarning = false;
		this.neverShowAgain = true;
		AppStorage.setItem(NEVER_SHOW_AGAIN_KEY, 'true');
	}

	reset(): void {
		this.showWarning = false;
		this.encoderInfo = null;
	}
}

export default new SoftwareEncoderWarning();

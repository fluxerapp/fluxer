// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('RuntimeCrash');

function toError(error: unknown): Error {
	if (error instanceof Error) {
		return error;
	}
	if (typeof error === 'string') {
		return new Error(error);
	}
	return new Error('Unknown runtime crash');
}

class RuntimeCrash {
	fatalError: Error | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	triggerFatalCrash(error: unknown): Error {
		const normalizedError = toError(error);
		if (this.fatalError) {
			return this.fatalError;
		}
		this.fatalError = normalizedError;
		logger.fatal('Triggering fatal runtime crash', normalizedError);
		return normalizedError;
	}
}

export default new RuntimeCrash();

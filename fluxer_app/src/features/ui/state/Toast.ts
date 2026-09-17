// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {ToastProps} from '@app/features/ui/toast';
import {makeAutoObservable, observableRef} from 'mobx';

const logger = new Logger('Toast');

interface ToastEntry {
	id: string;
	data: ToastProps;
}

class Toast {
	currentToast: ToastEntry | null = null;

	constructor() {
		makeAutoObservable(
			this,
			{
				currentToast: observableRef,
			},
			{autoBind: true},
		);
	}

	createToast(data: ToastProps): string {
		const id = crypto.randomUUID();
		logger.debug(`Creating toast: ${id}, type: ${data.type}`);
		this.currentToast = {id, data};
		return id;
	}

	destroyToast(id: string): void {
		if (this.currentToast?.id === id) {
			logger.debug(`Destroying toast: ${id}`);
			this.currentToast = null;
		}
	}

	success(message: string): string {
		return this.createToast({type: 'success', children: message, timeout: 3000});
	}

	error(message: string): string {
		return this.createToast({type: 'error', children: message, timeout: 5000});
	}

	getCurrentToast() {
		return this.currentToast;
	}

	hasToast(id: string): boolean {
		return this.currentToast?.id === id;
	}

	getToast(id: string): ToastProps | undefined {
		if (this.currentToast?.id === id) {
			return this.currentToast.data;
		}
		return undefined;
	}
}

export default new Toast();

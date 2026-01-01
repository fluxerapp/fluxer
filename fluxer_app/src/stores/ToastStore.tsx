/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable, observable} from 'mobx';
import type {ToastProps} from '~/components/uikit/Toast';
import {Logger} from '~/lib/Logger';

const logger = new Logger('ToastStore');

interface ToastEntry {
	id: string;
	data: ToastProps;
}

class ToastStore {
	currentToast: ToastEntry | null = null;

	constructor() {
		makeAutoObservable(
			this,
			{
				currentToast: observable.ref,
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

export default new ToastStore();

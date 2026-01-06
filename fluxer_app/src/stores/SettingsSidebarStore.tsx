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
import type React from 'react';

class SettingsSidebarStore {
	ownerId: string | null = null;
	overrideContent: React.ReactNode | null = null;
	useOverride = false;
	dismissedOwnerId: string | null = null;

	constructor() {
		makeAutoObservable(this, {overrideContent: observable.ref}, {autoBind: true});
	}

	get hasOverride(): boolean {
		return this.overrideContent != null;
	}

	setOverride(ownerId: string, content: React.ReactNode, options?: {defaultOn?: boolean}): void {
		this.ownerId = ownerId;
		this.overrideContent = content;
		this.dismissedOwnerId = null;
		this.useOverride = options?.defaultOn ?? false;
	}

	updateOverride(ownerId: string, content: React.ReactNode): void {
		if (this.ownerId && this.ownerId !== ownerId) return;
		this.overrideContent = content;
	}

	clearOverride(ownerId?: string): void {
		if (ownerId && this.ownerId && this.ownerId !== ownerId) return;
		this.ownerId = null;
		this.overrideContent = null;
		this.dismissedOwnerId = null;
		this.useOverride = false;
	}

	dismissOverride(ownerId?: string): void {
		if (ownerId && this.ownerId && this.ownerId !== ownerId) return;
		const targetOwnerId = ownerId ?? this.ownerId;
		this.useOverride = false;
		this.dismissedOwnerId = targetOwnerId;
	}

	setUseOverride(value: boolean): void {
		if (!this.hasOverride) {
			this.useOverride = false;
			return;
		}
		if (value) {
			this.dismissedOwnerId = null;
		}
		this.useOverride = value;
	}

	activateOverride(ownerId?: string): void {
		if (!this.hasOverride) return;
		if (ownerId && this.ownerId && this.ownerId !== ownerId) return;
		this.useOverride = true;
		this.dismissedOwnerId = null;
	}

	isDismissed(ownerId?: string): boolean {
		if (!this.dismissedOwnerId) return false;
		if (!ownerId) return this.ownerId === this.dismissedOwnerId;
		return ownerId === this.dismissedOwnerId;
	}
}

export default new SettingsSidebarStore();

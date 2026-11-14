// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable, observableRef} from 'mobx';
import type React from 'react';

class SettingsSidebar {
	ownerId: string | null = null;
	overrideContent: React.ReactNode | null = null;
	useOverride = false;
	dismissedOwnerId: string | null = null;

	constructor() {
		makeAutoObservable(this, {overrideContent: observableRef}, {autoBind: true});
	}

	get hasOverride(): boolean {
		return this.overrideContent != null;
	}

	setOverride(
		ownerId: string,
		content: React.ReactNode,
		options?: {
			defaultOn?: boolean;
		},
	): void {
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

export default new SettingsSidebar();

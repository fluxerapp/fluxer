// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	StreamSettingsShareContext,
	WindowShareAudioScope,
} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import {makeAutoObservable} from 'mobx';

export interface ActiveScreenShareSourceOptions {
	readonly isOwnWindow?: boolean;
}

export type PublishedScreenShareSource = 'web' | 'wayland' | 'device' | 'app' | 'display';

class ActiveScreenShareSource {
	sourceId: string | null = null;
	ownWindow = false;
	publishedSource: PublishedScreenShareSource | null = null;
	windowAudioScope: WindowShareAudioScope = 'window';
	pendingWindowAudioScope: WindowShareAudioScope | null = null;
	sourceDimensions: {width: number; height: number} | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setPublishedSource(
		publishedSource: PublishedScreenShareSource,
		sourceId: string | null,
		options: ActiveScreenShareSourceOptions = {},
	): void {
		this.publishedSource = publishedSource;
		this.sourceId = sourceId;
		this.ownWindow = sourceId !== null && options.isOwnWindow === true;
	}

	getSourceId(): string | null {
		return this.sourceId;
	}

	isOwnWindow(): boolean {
		return this.ownWindow;
	}

	getPublishedSource(): PublishedScreenShareSource | null {
		return this.publishedSource;
	}

	setSourceDimensions(dimensions: {width: number; height: number} | null): void {
		this.sourceDimensions = dimensions;
	}

	getSourceDimensions(): {width: number; height: number} | null {
		return this.sourceDimensions;
	}

	getWindowAudioScope(): WindowShareAudioScope {
		return this.windowAudioScope;
	}

	setWindowAudioScope(scope: WindowShareAudioScope): void {
		this.windowAudioScope = scope;
	}

	getPendingWindowAudioScope(): WindowShareAudioScope {
		return this.pendingWindowAudioScope ?? this.windowAudioScope;
	}

	setPendingWindowAudioScope(scope: WindowShareAudioScope): void {
		this.pendingWindowAudioScope = scope;
	}

	clearPendingWindowAudioScope(): void {
		this.pendingWindowAudioScope = null;
	}

	getShareContext(): StreamSettingsShareContext | null {
		if (this.publishedSource === 'app') return 'app';
		if (this.publishedSource === 'device') return 'device';
		if (this.publishedSource === null) return null;
		return 'display';
	}

	clear(): void {
		this.sourceId = null;
		this.ownWindow = false;
		this.publishedSource = null;
		this.windowAudioScope = 'window';
		this.pendingWindowAudioScope = null;
		this.sourceDimensions = null;
	}
}

export default new ActiveScreenShareSource();

// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {fetchReportFlow} from '@app/features/moderation/commands/ReportFlowCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	ReportFlowResponse,
	ReportFlowSurface,
	ReportFlowTargetType,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('ReportFlows');

export type ReportFlowLoadState =
	| {status: 'loading'; productName: string}
	| {status: 'loaded'; flow: ReportFlowResponse; productName: string}
	| {status: 'error'};

function cacheKey(targetType: ReportFlowTargetType, surface: ReportFlowSurface, locale: string): string {
	return `${targetType}:${surface}:${locale}`;
}

class ReportFlows {
	entries = new Map<string, ReportFlowLoadState>();
	private pending = new Map<string, Promise<ReportFlowResponse | null>>();

	constructor() {
		makeAutoObservable<this, 'pending'>(this, {pending: false}, {autoBind: true});
	}

	getState(targetType: ReportFlowTargetType, surface: ReportFlowSurface, locale: string): ReportFlowLoadState | null {
		return this.entries.get(cacheKey(targetType, surface, locale)) ?? null;
	}

	load(
		targetType: ReportFlowTargetType,
		surface: ReportFlowSurface,
		locale: string,
	): Promise<ReportFlowResponse | null> {
		const key = cacheKey(targetType, surface, locale);
		const current = this.entries.get(key);
		if (current?.status === 'loaded') return Promise.resolve(current.flow);
		const inFlight = this.pending.get(key);
		if (inFlight) return inFlight;
		this.entries.set(key, {status: 'loading', productName: RuntimeConfig.productName});
		return this.fetchFlow(key, targetType, surface, locale, {status: 'error'});
	}

	reload(
		targetType: ReportFlowTargetType,
		surface: ReportFlowSurface,
		locale: string,
	): Promise<ReportFlowResponse | null> {
		const key = cacheKey(targetType, surface, locale);
		this.pending.delete(key);
		this.entries.delete(key);
		return this.load(targetType, surface, locale);
	}

	revalidate(
		targetType: ReportFlowTargetType,
		surface: ReportFlowSurface,
		locale: string,
	): Promise<ReportFlowResponse | null> {
		const request = this.load(targetType, surface, locale);
		this.syncProductName(targetType, surface, locale);
		return request;
	}

	restart(
		targetType: ReportFlowTargetType,
		surface: ReportFlowSurface,
		locale: string,
	): Promise<ReportFlowResponse | null> {
		const request = this.reload(targetType, surface, locale);
		this.syncProductName(targetType, surface, locale);
		return request;
	}

	private syncProductName(targetType: ReportFlowTargetType, surface: ReportFlowSurface, locale: string): void {
		RuntimeConfig.revalidateDiscovery().then(
			() => this.refreshIfRenamed(targetType, surface, locale),
			(error: unknown) => logger.warn('Failed to refresh discovery for a report flow:', error),
		);
	}

	private refreshIfRenamed(targetType: ReportFlowTargetType, surface: ReportFlowSurface, locale: string): void {
		const key = cacheKey(targetType, surface, locale);
		const current = this.entries.get(key);
		if (current?.status !== 'loaded' || current.productName === RuntimeConfig.productName) return;
		if (this.pending.has(key)) return;
		void this.fetchFlow(key, targetType, surface, locale, current);
	}

	private fetchFlow(
		key: string,
		targetType: ReportFlowTargetType,
		surface: ReportFlowSurface,
		locale: string,
		failedState: ReportFlowLoadState,
	): Promise<ReportFlowResponse | null> {
		const request: Promise<ReportFlowResponse | null> = fetchReportFlow(targetType, surface, locale).then(
			(flow) =>
				this.settle(key, request, {status: 'loaded', flow, productName: RuntimeConfig.productName}) ? flow : null,
			(error: unknown) => {
				logger.error('Failed to load report flow:', error);
				this.settle(key, request, failedState);
				return null;
			},
		);
		this.pending.set(key, request);
		return request;
	}

	private settle(key: string, request: Promise<ReportFlowResponse | null>, state: ReportFlowLoadState): boolean {
		if (this.pending.get(key) !== request) return false;
		this.pending.delete(key);
		this.entries.set(key, state);
		return true;
	}

	reset(): void {
		this.pending.clear();
		this.entries.clear();
	}
}

export default new ReportFlows();

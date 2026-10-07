// SPDX-License-Identifier: AGPL-3.0-or-later

import DesktopRuntimeTransactions, {
	type CommittedDesktopRuntime,
	DesktopRuntimeTransactionError,
	type PreparedDesktopRuntime,
	requiresDesktopRuntimeTransaction,
	runtimeTransportApiEndpoint,
} from '@app/features/app/state/DesktopRuntimeTransaction';
import {
	type GifProvider,
	type GifProviderInfo,
	type GifProviderInfoInput,
	normalizeGifProviderInfo,
} from '@app/features/app/state/GifProviderConfig';
import InstanceSnapshotStore, {
	type InstanceSnapshotResolution,
	type InstanceSnapshotResolveRequest,
	type RuntimeConfigSnapshot,
	runtimeInstanceKey,
} from '@app/features/app/state/InstanceSnapshotStore';
import {requireRuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';
import {
	accountIdentityOf,
	tagStyleOf,
	usesUniqueUsernames,
	usesUsernameSignIn,
} from '@app/features/app/utils/AccountIdentityFeatures';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {
	type AccountIdentityMode,
	AccountIdentityModes,
	type TagStyle,
} from '@fluxer/constants/src/AccountIdentityConstants';
import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import {InstanceDiscoveryUnreachableError} from '@fluxer/instance_bootstrap/src/Discovery';
import {InstanceEndpointKind, normalizeInstanceEndpoint} from '@fluxer/instance_bootstrap/src/EndpointNormalization';
import {isOfficialInstanceHost, OFFICIAL_INSTANCE_DISPLAY_HOST} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import type {
	InstanceAgePolicy,
	InstanceAppPublic,
	InstanceCommunity,
	InstanceDiscoveryResponse,
	InstanceFeatures,
	InstanceRegistration,
	InstanceServices,
	InstanceSso as InstanceSsoConfig,
} from '@fluxer/instance_bootstrap/src/Types';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {makeAutoObservable, observableRef} from 'mobx';

const logger = new Logger('RuntimeConfig');

export {
	runtimeConfigSnapshotsAreSameInstance,
	runtimeInstanceKey,
} from '@app/features/app/state/InstanceSnapshotStore';
export type {
	GifProvider,
	GifProviderInfo,
	InstanceCommunity,
	InstanceDiscoveryResponse,
	InstanceFeatures,
	InstanceRegistration,
	InstanceServices,
	InstanceSnapshotResolution,
	InstanceSnapshotResolveRequest,
	InstanceSsoConfig,
	RuntimeConfigSnapshot,
};

export interface ApplyRuntimeConfigSnapshotRequest {
	snapshot: RuntimeConfigSnapshot;
	signal: AbortSignal | null;
}

export interface PreparedRuntimeConfig {
	readonly snapshot: RuntimeConfigSnapshot;
	readonly transportApiEndpoint: string;
	readonly expectedGeneration: number;
	readonly desktopRuntime: PreparedDesktopRuntime | null;
}

export interface CommittedRuntimeConfig {
	readonly snapshot: RuntimeConfigSnapshot;
	readonly transportApiEndpoint: string;
	readonly expectedGeneration: number;
	readonly previousRuntime: ActiveRuntime | null;
	readonly activeRuntime: ActiveRuntime;
	readonly desktopRuntime: CommittedDesktopRuntime | null;
	readonly publication: RuntimeConfigPublication;
}

interface GifProviderOverride {
	readonly instanceKey: string;
	readonly info: GifProviderInfo;
}

interface ActiveRuntime {
	readonly snapshot: RuntimeConfigSnapshot;
	readonly transportApiEndpoint: string;
}

type RuntimeConfigPublicationPhase = 'committed' | 'published' | 'renderer-rolled-back' | 'rolled-back' | 'finalized';

interface RuntimeConfigPublication {
	phase: RuntimeConfigPublicationPhase;
}

const DEFAULT_PREMIUM_PRODUCT_NAME = 'Plutonium';

class RuntimeActivationSupersededError extends Error {
	constructor() {
		super('Runtime activation was superseded by another instance transition');
		this.name = 'RuntimeActivationSupersededError';
	}
}

class RuntimeInstanceIdentityChangedError extends Error {
	constructor(expectedInstanceKey: string, resolvedInstanceKey: string) {
		super(`Runtime discovery resolved ${resolvedInstanceKey} for stored instance ${expectedInstanceKey}`);
		this.name = 'RuntimeInstanceIdentityChangedError';
	}
}

class RuntimeCommitRollbackError extends AggregateError {
	constructor(commitError: unknown, rollbackError: unknown) {
		super([commitError, rollbackError], 'Desktop runtime commit failed and its native routing could not be restored');
		this.name = 'RuntimeCommitRollbackError';
	}
}

class RuntimeConfigUnavailableError extends Error {
	constructor() {
		super('No instance runtime is active');
		this.name = 'RuntimeConfigUnavailableError';
	}
}

function nextLifecycleGeneration(generation: number): number {
	if (!Number.isSafeInteger(generation) || generation < 0 || generation >= Number.MAX_SAFE_INTEGER) {
		throw new Error('Runtime config lifecycle generation is exhausted');
	}
	return generation + 1;
}

export function describeAPIEndpoint(endpoint: string): string {
	const normalized = normalizeInstanceEndpoint(endpoint, InstanceEndpointKind.API);
	if (normalized == null || !normalized.startsWith('https://')) {
		return normalized ?? endpoint;
	}
	const withoutScheme = normalized.slice('https://'.length);
	const pathStart = withoutScheme.indexOf('/');
	const host = pathStart === -1 ? withoutScheme : withoutScheme.slice(0, pathStart);
	const path = pathStart === -1 ? '' : withoutScheme.slice(pathStart);
	if (isOfficialInstanceHost(host)) {
		return `${OFFICIAL_INSTANCE_DISPLAY_HOST}${path}`;
	}
	return withoutScheme;
}

const DISCOVERY_REVALIDATION_TIMEOUT_MS = 5000;

let pendingDiscoveryRevalidation: Promise<void> | null = null;

class RuntimeConfig {
	private lifecycleGeneration = 0;
	private activeRuntime: ActiveRuntime | null = null;
	private retainedSnapshot: RuntimeConfigSnapshot | null = null;
	private gifProviderOverride: GifProviderOverride | null = null;

	constructor() {
		makeAutoObservable<
			RuntimeConfig,
			'activeRuntime' | 'gifProviderOverride' | 'isCurrentGeneration' | 'lifecycleGeneration' | 'retainedSnapshot'
		>(
			this,
			{
				isCurrentGeneration: false,
				activeRuntime: observableRef,
				retainedSnapshot: observableRef,
				gifProviderOverride: observableRef,
				lifecycleGeneration: false,
			},
			{autoBind: true},
		);
	}

	get apiEndpoint(): string {
		return this.readableSnapshot().apiEndpoint;
	}

	get transportApiEndpoint(): string {
		return this.requireActiveRuntime().transportApiEndpoint;
	}

	get uploadRelayEndpoint(): string | null {
		return this.readableSnapshot().uploadRelayEndpoint;
	}

	get apiPublicEndpoint(): string {
		return this.readableSnapshot().apiPublicEndpoint;
	}

	get gatewayEndpoint(): string {
		return this.readableSnapshot().gatewayEndpoint;
	}

	get mediaEndpoint(): string {
		return this.readableSnapshot().mediaEndpoint;
	}

	get staticCdnEndpoint(): string {
		return this.readableSnapshot().staticCdnEndpoint;
	}

	get marketingEndpoint(): string {
		return this.readableSnapshot().marketingEndpoint;
	}

	get adminEndpoint(): string {
		return this.readableSnapshot().adminEndpoint;
	}

	get inviteEndpoint(): string {
		return this.readableSnapshot().inviteEndpoint;
	}

	get giftEndpoint(): string {
		return this.readableSnapshot().giftEndpoint;
	}

	get webAppEndpoint(): string {
		return this.readableSnapshot().webAppEndpoint;
	}

	get gifProvider(): GifProvider {
		return this.activeGifProviderOverride()?.name ?? this.readableSnapshot().gifProvider;
	}

	get gifProviderDisplayName(): string {
		return this.activeGifProviderOverride()?.displayName ?? this.readableSnapshot().gifProviderDisplayName;
	}

	get gifAttributionRequired(): boolean {
		return this.activeGifProviderOverride()?.attributionRequired ?? this.readableSnapshot().gifAttributionRequired;
	}

	private activeGifProviderOverride(): GifProviderInfo | null {
		const override = this.gifProviderOverride;
		const snapshot = this.activeRuntime?.snapshot;
		if (override === null || snapshot === undefined || runtimeInstanceKey(snapshot) !== override.instanceKey) {
			return null;
		}
		return override.info;
	}

	applyGifProviderHeaders(input: GifProviderInfoInput): void {
		const snapshot = this.activeRuntime?.snapshot;
		const instanceKey = snapshot === undefined ? null : runtimeInstanceKey(snapshot);
		if (instanceKey === null) {
			return;
		}
		const info = normalizeGifProviderInfo(input);
		if (
			this.gifProvider === info.name &&
			this.gifProviderDisplayName === info.displayName &&
			this.gifAttributionRequired === info.attributionRequired
		) {
			return;
		}
		this.gifProviderOverride = {instanceKey, info};
	}

	get apiCodeVersion(): number {
		return this.readableSnapshot().apiCodeVersion;
	}

	get features(): InstanceFeatures {
		return this.readableSnapshot().features;
	}

	get sso(): InstanceSsoConfig | null {
		return this.readableSnapshot().sso;
	}

	get registration(): InstanceRegistration {
		return this.readableSnapshot().registration;
	}

	get community(): InstanceCommunity {
		return this.readableSnapshot().community;
	}

	get services(): InstanceServices {
		return this.readableSnapshot().services;
	}

	get publicPushVapidKey(): string | null {
		return this.readableSnapshot().publicPushVapidKey;
	}

	get limits(): LimitConfigSnapshot {
		return this.readableSnapshot().limits;
	}

	get appPublic(): InstanceAppPublic {
		return this.readableSnapshot().appPublic;
	}

	get agePolicy(): InstanceAgePolicy | null {
		return this.readableSnapshot().agePolicy ?? null;
	}

	private requireActiveSnapshot(): RuntimeConfigSnapshot {
		return this.requireActiveRuntime().snapshot;
	}

	private readableSnapshot(): RuntimeConfigSnapshot {
		const snapshot = this.activeRuntime?.snapshot ?? this.retainedSnapshot;
		if (snapshot === null) {
			throw new RuntimeConfigUnavailableError();
		}
		return snapshot;
	}

	private requireActiveRuntime(): ActiveRuntime {
		const runtime = this.activeRuntime;
		if (runtime === null) {
			throw new RuntimeConfigUnavailableError();
		}
		return runtime;
	}

	getSnapshotOrNull(): RuntimeConfigSnapshot | null {
		return this.activeRuntime?.snapshot ?? null;
	}

	getSnapshot(): RuntimeConfigSnapshot {
		return this.readableSnapshot();
	}

	async deactivate(): Promise<void> {
		const expectedGeneration = this.lifecycleGeneration;
		const runtime = this.activeRuntime;
		if (runtime === null) {
			return;
		}
		const activeInstanceKey = runtimeInstanceKey(runtime.snapshot);
		if (activeInstanceKey === null) {
			throw new Error('Active runtime has no usable instance identity');
		}
		const desktop = requiresDesktopRuntimeTransaction();
		if (desktop) {
			await DesktopRuntimeTransactions.deactivate(activeInstanceKey);
		}
		if (!this.isCurrentGeneration(expectedGeneration) || this.activeRuntime !== runtime) {
			throw new RuntimeActivationSupersededError();
		}
		if (desktop) {
			this.retainedSnapshot = runtime.snapshot;
			this.activeRuntime = null;
		}
		this.lifecycleGeneration = nextLifecycleGeneration(this.lifecycleGeneration);
	}

	private isCurrentGeneration(generation: number): boolean {
		return this.lifecycleGeneration === generation;
	}

	applySnapshot(snapshot: RuntimeConfigSnapshot): void {
		if (requiresDesktopRuntimeTransaction()) {
			throw new DesktopRuntimeTransactionError('Desktop runtime changes must use applySnapshotAndWaitForDesktop');
		}
		const prepared = this.prepareLocalSnapshot(snapshot, this.lifecycleGeneration);
		const committed = this.commitLocalPreparation(prepared);
		this.publishCommittedSnapshot(committed);
		committed.publication.phase = 'finalized';
	}

	async applySnapshotAndWaitForDesktop({snapshot, signal}: ApplyRuntimeConfigSnapshotRequest): Promise<void> {
		const prepared = await this.prepareSnapshot({snapshot, signal});
		const committed = await this.commitPreparedSnapshot(prepared);
		try {
			this.publishCommittedSnapshot(committed);
		} catch (error) {
			return await this.rollbackFailedCommit(committed, error);
		}
		try {
			await this.finalizeCommittedSnapshot(committed);
		} catch (error) {
			return await this.rollbackFailedCommit(committed, error);
		}
	}

	async prepareSnapshot({snapshot, signal}: ApplyRuntimeConfigSnapshotRequest): Promise<PreparedRuntimeConfig> {
		signal?.throwIfAborted();
		const usable = this.requireUsableSnapshot(snapshot);
		this.assertCodeVersion(usable.apiCodeVersion);
		const generation = this.lifecycleGeneration;
		let desktopRuntime: PreparedDesktopRuntime | null = null;
		try {
			if (requiresDesktopRuntimeTransaction()) {
				desktopRuntime = await DesktopRuntimeTransactions.prepare(usable, signal);
			}
			signal?.throwIfAborted();
			if (!this.isCurrentGeneration(generation)) {
				throw new RuntimeActivationSupersededError();
			}
			if (desktopRuntime !== null) {
				const preparedSnapshot = this.requireUsableSnapshot(desktopRuntime.snapshot);
				this.assertCodeVersion(preparedSnapshot.apiCodeVersion);
				return {
					snapshot: preparedSnapshot,
					transportApiEndpoint: desktopRuntime.transportApiEndpoint,
					expectedGeneration: generation,
					desktopRuntime,
				};
			}
			return this.prepareLocalSnapshot(usable, generation);
		} catch (error) {
			if (desktopRuntime !== null) {
				try {
					await DesktopRuntimeTransactions.abort(desktopRuntime);
				} catch (rollbackError) {
					throw new RuntimeCommitRollbackError(error, rollbackError);
				}
			}
			throw error;
		}
	}

	async resolveAndPrepareSnapshot({
		snapshot,
		signal,
	}: ApplyRuntimeConfigSnapshotRequest): Promise<PreparedRuntimeConfig> {
		const expectedInstanceKey = runtimeInstanceKey(this.requireUsableSnapshot(snapshot));
		if (expectedInstanceKey === null) {
			throw new Error('Runtime resolution requires a usable instance key');
		}
		let resolution: InstanceSnapshotResolution;
		try {
			resolution = await this.resolveEndpoint({input: snapshot.apiEndpoint, signal});
		} catch (error) {
			if (!(error instanceof InstanceDiscoveryUnreachableError)) {
				throw error;
			}
			signal?.throwIfAborted();
			logger.warn('Instance discovery is unreachable, preparing the stored instance snapshot', error);
			return await this.prepareSnapshot({snapshot, signal});
		}
		if (resolution.instanceKey !== expectedInstanceKey) {
			throw new RuntimeInstanceIdentityChangedError(expectedInstanceKey, resolution.instanceKey);
		}
		return await this.prepareSnapshot({snapshot: resolution.snapshot, signal});
	}

	async commitPreparedSnapshot(prepared: PreparedRuntimeConfig): Promise<CommittedRuntimeConfig> {
		try {
			this.requirePreparedRuntime(prepared);
		} catch (error) {
			try {
				await this.abortPreparedSnapshot(prepared);
			} catch (rollbackError) {
				throw new RuntimeCommitRollbackError(error, rollbackError);
			}
			throw error;
		}
		const desktopRuntime = prepared.desktopRuntime;
		if (desktopRuntime === null) {
			return this.commitLocalPreparation(prepared);
		}
		const committedDesktopRuntime = await DesktopRuntimeTransactions.commit(desktopRuntime);
		try {
			return this.createCommittedRuntime(prepared, committedDesktopRuntime);
		} catch (error) {
			try {
				await DesktopRuntimeTransactions.rollback(committedDesktopRuntime);
			} catch (rollbackError) {
				throw new RuntimeCommitRollbackError(error, rollbackError);
			}
			throw error;
		}
	}

	publishCommittedSnapshot(committed: CommittedRuntimeConfig): void {
		if (committed.publication.phase !== 'committed') {
			throw new Error(`Runtime config commit cannot publish from ${committed.publication.phase}`);
		}
		if (!this.isCurrentGeneration(committed.expectedGeneration)) {
			throw new RuntimeActivationSupersededError();
		}
		if (this.activeRuntime !== committed.previousRuntime) {
			throw new RuntimeActivationSupersededError();
		}
		const snapshot = this.requireUsableSnapshot(committed.snapshot);
		this.assertCodeVersion(snapshot.apiCodeVersion);
		const transportApiEndpoint = runtimeTransportApiEndpoint(snapshot);
		if (committed.transportApiEndpoint !== transportApiEndpoint) {
			throw new DesktopRuntimeTransactionError('Committed runtime transport does not match its instance snapshot');
		}
		if (
			committed.activeRuntime.snapshot !== committed.snapshot ||
			committed.activeRuntime.transportApiEndpoint !== committed.transportApiEndpoint
		) {
			throw new Error('Runtime config commit contains a different renderer runtime');
		}
		const publishedGeneration = nextLifecycleGeneration(this.lifecycleGeneration);
		if (committed.desktopRuntime !== null) {
			DesktopRuntimeTransactions.publish(committed.desktopRuntime);
		}
		this.activeRuntime = committed.activeRuntime;
		this.lifecycleGeneration = publishedGeneration;
		committed.publication.phase = 'published';
	}

	async abortPreparedSnapshot(prepared: PreparedRuntimeConfig): Promise<void> {
		if (prepared.desktopRuntime !== null) {
			await DesktopRuntimeTransactions.abort(prepared.desktopRuntime);
		}
	}

	async finalizeCommittedSnapshot(committed: CommittedRuntimeConfig): Promise<void> {
		if (committed.publication.phase !== 'published') {
			throw new Error(`Runtime config commit cannot finalize from ${committed.publication.phase}`);
		}
		if (committed.desktopRuntime !== null) {
			await DesktopRuntimeTransactions.finalize(committed.desktopRuntime);
		}
		committed.publication.phase = 'finalized';
	}

	rollbackPublishedSnapshot(committed: CommittedRuntimeConfig): void {
		if (committed.publication.phase === 'committed') {
			return;
		}
		if (committed.publication.phase !== 'published') {
			throw new Error(`Runtime config publication cannot roll back from ${committed.publication.phase}`);
		}
		const publishedGeneration = nextLifecycleGeneration(committed.expectedGeneration);
		if (this.lifecycleGeneration !== publishedGeneration || this.activeRuntime !== committed.activeRuntime) {
			throw new RuntimeActivationSupersededError();
		}
		const rollbackGeneration = nextLifecycleGeneration(this.lifecycleGeneration);
		if (committed.desktopRuntime !== null) {
			DesktopRuntimeTransactions.rollbackPublished(committed.desktopRuntime);
		}
		this.activeRuntime = committed.previousRuntime;
		this.lifecycleGeneration = rollbackGeneration;
		committed.publication.phase = 'renderer-rolled-back';
	}

	async rollbackCommittedSnapshot(committed: CommittedRuntimeConfig): Promise<void> {
		if (
			committed.publication.phase !== 'committed' &&
			committed.publication.phase !== 'published' &&
			committed.publication.phase !== 'renderer-rolled-back'
		) {
			throw new Error(`Runtime config commit cannot roll back from ${committed.publication.phase}`);
		}
		if (committed.desktopRuntime !== null) {
			await DesktopRuntimeTransactions.rollback(committed.desktopRuntime);
		}
		committed.publication.phase = 'rolled-back';
	}

	async resolveEndpoint(request: InstanceSnapshotResolveRequest): Promise<InstanceSnapshotResolution> {
		const resolution = await InstanceSnapshotStore.resolve(request);
		this.assertCodeVersion(resolution.snapshot.apiCodeVersion);
		return resolution;
	}

	private requireUsableSnapshot(snapshot: RuntimeConfigSnapshot): RuntimeConfigSnapshot {
		const validated = requireRuntimeConfigSnapshot(snapshot);
		if (runtimeInstanceKey(validated) === null) {
			throw new Error(`Runtime snapshot has no usable instance key (apiEndpoint: "${validated.apiEndpoint}")`);
		}
		return validated;
	}

	private prepareLocalSnapshot(snapshot: RuntimeConfigSnapshot, expectedGeneration: number): PreparedRuntimeConfig {
		const usable = this.requireUsableSnapshot(snapshot);
		this.assertCodeVersion(usable.apiCodeVersion);
		return {
			snapshot: usable,
			transportApiEndpoint: runtimeTransportApiEndpoint(usable),
			expectedGeneration,
			desktopRuntime: null,
		};
	}

	private requirePreparedRuntime(prepared: PreparedRuntimeConfig): RuntimeConfigSnapshot {
		if (!this.isCurrentGeneration(prepared.expectedGeneration)) {
			throw new RuntimeActivationSupersededError();
		}
		const snapshot = this.requireUsableSnapshot(prepared.snapshot);
		this.assertCodeVersion(snapshot.apiCodeVersion);
		const transportApiEndpoint = runtimeTransportApiEndpoint(snapshot);
		if (prepared.transportApiEndpoint !== transportApiEndpoint) {
			throw new DesktopRuntimeTransactionError('Prepared runtime transport does not match its instance snapshot');
		}
		if (requiresDesktopRuntimeTransaction() !== (prepared.desktopRuntime !== null)) {
			throw new DesktopRuntimeTransactionError('Prepared runtime does not match the renderer transport environment');
		}
		return snapshot;
	}

	private commitLocalPreparation(prepared: PreparedRuntimeConfig): CommittedRuntimeConfig {
		if (prepared.desktopRuntime !== null) {
			throw new DesktopRuntimeTransactionError('Desktop runtime preparation cannot be committed as a local runtime');
		}
		return this.createCommittedRuntime(prepared, null);
	}

	private createCommittedRuntime(
		prepared: PreparedRuntimeConfig,
		desktopRuntime: CommittedDesktopRuntime | null,
	): CommittedRuntimeConfig {
		const snapshot = this.requirePreparedRuntime(prepared);
		const activeRuntime = {snapshot, transportApiEndpoint: prepared.transportApiEndpoint};
		return {
			snapshot,
			transportApiEndpoint: prepared.transportApiEndpoint,
			expectedGeneration: prepared.expectedGeneration,
			previousRuntime: this.activeRuntime,
			activeRuntime,
			desktopRuntime,
			publication: {phase: 'committed'},
		};
	}

	private async rollbackFailedCommit(committed: CommittedRuntimeConfig, operationError: unknown): Promise<never> {
		const rollbackErrors: Array<unknown> = [];
		try {
			this.rollbackPublishedSnapshot(committed);
		} catch (error) {
			rollbackErrors.push(error);
		}
		try {
			await this.rollbackCommittedSnapshot(committed);
		} catch (error) {
			rollbackErrors.push(error);
		}
		if (rollbackErrors.length > 0) {
			throw new AggregateError(
				[operationError, ...rollbackErrors],
				'Runtime config commit failed and could not be rolled back completely',
			);
		}
		throw operationError;
	}

	applyAccountIdentity(mode: AccountIdentityMode, tagStyle: TagStyle): void {
		const current = this.requireActiveSnapshot();
		this.replaceActiveFeatures({
			...current.features,
			account_identity: mode,
			tag_style: tagStyle,
			emails_enabled: mode === AccountIdentityModes.USERNAME ? false : current.features.emails_enabled,
		});
	}

	async refreshDiscovery(signal?: AbortSignal): Promise<void> {
		const current = this.requireActiveSnapshot();
		const resolution = await InstanceSnapshotStore.refresh({input: current.apiEndpoint, signal: signal ?? null});
		if (runtimeInstanceKey(resolution.snapshot) !== runtimeInstanceKey(this.requireActiveSnapshot())) {
			return;
		}
		const runtime = this.requireActiveRuntime();
		const usable = this.requireUsableSnapshot({
			...runtime.snapshot,
			features: resolution.snapshot.features,
			appPublic: resolution.snapshot.appPublic,
		});
		this.activeRuntime = {...runtime, snapshot: usable};
		this.lifecycleGeneration = nextLifecycleGeneration(this.lifecycleGeneration);
	}

	revalidateDiscovery(): Promise<void> {
		if (pendingDiscoveryRevalidation !== null) return pendingDiscoveryRevalidation;
		const request = this.refreshDiscovery(AbortSignal.timeout(DISCOVERY_REVALIDATION_TIMEOUT_MS)).finally(() => {
			if (pendingDiscoveryRevalidation === request) pendingDiscoveryRevalidation = null;
		});
		pendingDiscoveryRevalidation = request;
		return request;
	}

	private replaceActiveFeatures(features: InstanceFeatures): void {
		const runtime = this.requireActiveRuntime();
		const usable = this.requireUsableSnapshot({...runtime.snapshot, features});
		this.activeRuntime = {...runtime, snapshot: usable};
		this.lifecycleGeneration = nextLifecycleGeneration(this.lifecycleGeneration);
	}

	applyAdminInstanceConfig(config: InstanceConfigResponse): void {
		const current = this.requireActiveSnapshot();
		const appPublic: InstanceAppPublic = {
			branding: config.app_public.branding,
			setup: {
				configured: config.app_public.setup.configured,
				admin_url: current.appPublic.setup.admin_url,
			},
			legal: config.app_public.legal,
			registration: config.app_public.registration,
		};
		const snapshot: RuntimeConfigSnapshot = {
			...current,
			features: {
				...current.features,
				self_hosted: config.self_hosted,
				premium_enabled: !config.self_hosted || config.policy.premium_mode === 'mirror',
				stripe_enabled: config.billing.billing_active,
				stripe_serviceable: config.billing.stripe_serviceable,
				account_identity: config.account_identity.mode,
				tag_style: config.account_identity.tag_style,
				emails_enabled:
					config.account_identity.mode === AccountIdentityModes.USERNAME ? false : current.features.emails_enabled,
			},
			registration: {
				mode: config.registration.mode,
				admin_registration_urls_enabled: config.registration.admin_registration_urls_enabled,
			},
			community: {
				single_community: config.policy.single_community_enabled,
				single_community_guild_id: config.policy.single_community_enabled
					? config.policy.single_community_guild_id
					: null,
				direct_messages_disabled: config.policy.direct_messages_disabled,
				guild_create_access: config.policy.guild_create_access,
			},
			services: {
				gif_enabled: config.policy.services_resolved.gif_enabled,
				youtube_enabled: config.policy.services_resolved.youtube_enabled,
				bluesky_enabled: config.policy.services_resolved.bluesky_enabled,
			},
			appPublic,
		};
		const usable = this.requireUsableSnapshot(snapshot);
		const runtime = this.requireActiveRuntime();
		this.activeRuntime = {...runtime, snapshot: usable};
		this.lifecycleGeneration = nextLifecycleGeneration(this.lifecycleGeneration);
	}

	private assertCodeVersion(instanceVersion: number): void {
		if (!Number.isSafeInteger(instanceVersion) || instanceVersion <= 0) {
			throw new Error(`Invalid server code version: ${instanceVersion}`);
		}
		if (instanceVersion < API_CODE_VERSION) {
			throw new Error(
				`Incompatible server (code version ${instanceVersion}); this client requires ${API_CODE_VERSION}.`,
			);
		}
	}

	get webAppBaseUrl(): string {
		return this.webAppEndpoint;
	}

	get statusPageUrl(): string {
		return this.activeRuntime?.snapshot.appPublic.branding.status_page_url ?? '';
	}

	get statusPageIncidentHistoryUrl(): string {
		return this.activeRuntime?.snapshot.appPublic.branding.status_page_incident_history_url ?? '';
	}

	isSelfHosted(): boolean {
		return DeveloperOptions.selfHostedModeOverride || this.features.self_hosted;
	}

	get premiumEnabled(): boolean {
		return this.features.premium_enabled;
	}

	get stripeEnabled(): boolean {
		return this.features.stripe_enabled;
	}

	get stripeServiceable(): boolean {
		return this.features.stripe_serviceable;
	}

	get premiumProductName(): string {
		return this.appPublic.branding.premium_product_name?.trim() || DEFAULT_PREMIUM_PRODUCT_NAME;
	}

	get premiumInfoUrl(): string | null {
		return this.appPublic.branding.premium_info_url ?? null;
	}

	get emailsEnabled(): boolean {
		return this.features.emails_enabled;
	}

	get accountIdentity(): AccountIdentityMode {
		return accountIdentityOf(this.features);
	}

	get usesUsernameSignIn(): boolean {
		return usesUsernameSignIn(this.features);
	}

	get tagStyle(): TagStyle {
		return tagStyleOf(this.features);
	}

	get usesUniqueUsernames(): boolean {
		return usesUniqueUsernames(this.features);
	}

	get productName(): string {
		return this.appPublic.branding.product_name;
	}

	get iconUrl(): string | null {
		return this.appPublic.branding.icon_url;
	}

	get symbolUrl(): string | null {
		return this.appPublic.branding.symbol_url;
	}

	get logoUrl(): string | null {
		return this.appPublic.branding.logo_url;
	}

	get wordmarkUrl(): string | null {
		return this.appPublic.branding.wordmark_url;
	}

	get faviconUrl(): string | null {
		return this.appPublic.branding.favicon_url;
	}

	get themeColor(): string | null {
		return this.appPublic.branding.theme_color;
	}

	get termsUrl(): string | null {
		return this.appPublic.legal.terms_url;
	}

	get privacyUrl(): string | null {
		return this.appPublic.legal.privacy_url;
	}

	get collectDateOfBirthOnRegistration(): boolean {
		return this.appPublic.registration.collect_date_of_birth;
	}

	get setupAdminUrl(): string | null {
		return this.appPublic.setup.admin_url;
	}

	requiresSelfHostedSetup(): boolean {
		if (getElectronAPI() != null) {
			return false;
		}
		if (this.activeRuntime === null) {
			return false;
		}
		return this.isSelfHosted() && !this.appPublic.setup.configured;
	}

	get singleCommunityEnabled(): boolean {
		return this.community.single_community;
	}

	get singleCommunityGuildId(): string | null {
		return this.community.single_community ? this.community.single_community_guild_id : null;
	}

	get directMessagesDisabled(): boolean {
		return this.community.direct_messages_disabled;
	}

	get gifEnabled(): boolean {
		return this.services.gif_enabled;
	}

	get blueskyConnectionsEnabled(): boolean {
		return this.services.bluesky_enabled;
	}

	get marketingHost(): string {
		return new URL(this.marketingEndpoint).host;
	}

	get inviteHost(): string {
		return new URL(this.inviteEndpoint).host;
	}

	get giftHost(): string {
		return new URL(this.giftEndpoint).host;
	}

	get inviteUrlBase(): string {
		const url = new URL(this.inviteEndpoint);
		const path = url.pathname === '/' ? '' : url.pathname;
		return `${url.host}${path}`;
	}

	get giftUrlBase(): string {
		const url = new URL(this.giftEndpoint);
		const path = url.pathname === '/' ? '' : url.pathname;
		return `${url.host}${path}`;
	}

	get localInstanceDomain(): string {
		return new URL(this.apiEndpoint).hostname;
	}
}

export default new RuntimeConfig();

// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {REQUEST_ID_MAX_LENGTH, readBoundedString, readExactPlainRecord} from '@electron/common/PlainRecord';
import {resolveDesktopRuntimePlan} from '@electron/main/DesktopRuntimeDiscovery';
import {projectDesktopRuntimePlan} from '@electron/main/DesktopRuntimePlanProjection';
import {getDesktopLocalAppProtocol} from '@electron/main/LocalAppProtocol';
import type {LocalAppRuntimePlan} from '@electron/main/LocalAppRuntimePlans';
import type {RendererDocumentOwnerFactory} from '@electron/main/RendererDocumentOwner';
import {
	isSameRendererDocument,
	type RendererDocumentInvalidationReason,
	type RendererDocumentIpcEvent,
	type RendererDocumentOwner,
	type RendererDocumentOwnerWatcher,
} from '@electron/main/RendererDocumentOwnership';
import type {
	DesktopRuntimeAbort,
	DesktopRuntimeCommit,
	DesktopRuntimeDeactivation,
	DesktopRuntimeFinalization,
	DesktopRuntimePreparation,
	DesktopRuntimeRollback,
} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';

const log = createChildLogger('DesktopRuntimeTransaction');

const MAX_RUNTIME_PREPARATIONS = 8;
const INSTANCE_KEY_MAX_LENGTH = 8192;

const PREPARE_REQUEST_KEYS: ReadonlyArray<string> = Object.freeze(['preparationId', 'instanceKey']);
const TRANSACTION_REQUEST_KEYS: ReadonlyArray<string> = Object.freeze(['preparationId', 'instanceKey', 'baseRevision']);
const COMMITTED_TRANSACTION_REQUEST_KEYS: ReadonlyArray<string> = Object.freeze([
	'preparationId',
	'instanceKey',
	'baseRevision',
	'committedRevision',
]);
const DEACTIVATE_REQUEST_KEYS: ReadonlyArray<string> = Object.freeze(['rendererActiveInstanceKey']);

class InvalidDesktopRuntimeTransactionRequestError extends TypeError {
	public constructor(context: string) {
		super(`Desktop runtime transaction ${context} is malformed`);
		this.name = 'InvalidDesktopRuntimeTransactionRequestError';
	}
}

class DesktopRuntimePreparationCapacityError extends Error {
	public constructor() {
		super('Desktop runtime preparation capacity is exhausted');
		this.name = 'DesktopRuntimePreparationCapacityError';
	}
}

class DesktopRuntimeInstanceMismatchError extends Error {
	public constructor(requested: string, resolved: string) {
		super(`Desktop runtime preparation requested ${requested} but discovery resolved ${resolved}`);
		this.name = 'DesktopRuntimeInstanceMismatchError';
	}
}

class DesktopRuntimeActivationRevisionExhaustedError extends Error {
	public constructor() {
		super('Desktop runtime activation revision is exhausted');
		this.name = 'DesktopRuntimeActivationRevisionExhaustedError';
	}
}

class DesktopRuntimePreparationNotFoundError extends Error {
	public constructor(preparationId: string) {
		super(`Desktop runtime preparation ${preparationId} does not exist`);
		this.name = 'DesktopRuntimePreparationNotFoundError';
	}
}

class DesktopRuntimePreparationAlreadyExistsError extends Error {
	public constructor(preparationId: string) {
		super(`Desktop runtime preparation ${preparationId} already exists`);
		this.name = 'DesktopRuntimePreparationAlreadyExistsError';
	}
}

class DesktopRuntimeCommitInProgressError extends Error {
	public constructor(preparationId: string) {
		super(`Desktop runtime preparation ${preparationId} cannot commit while another runtime commit is provisional`);
		this.name = 'DesktopRuntimeCommitInProgressError';
	}
}

class DesktopRuntimeDeactivationInProgressError extends Error {
	public constructor() {
		super('Desktop runtime cannot deactivate while another runtime transition is active');
		this.name = 'DesktopRuntimeDeactivationInProgressError';
	}
}

class DesktopRuntimeDeactivationIdentityError extends Error {
	public constructor() {
		super('Desktop runtime deactivation does not match the renderer runtime identity');
		this.name = 'DesktopRuntimeDeactivationIdentityError';
	}
}

class DesktopRuntimePreparationOwnershipError extends Error {
	public constructor(preparationId: string) {
		super(`Desktop runtime preparation ${preparationId} belongs to another renderer document`);
		this.name = 'DesktopRuntimePreparationOwnershipError';
	}
}

class DesktopRuntimePreparationIdentityError extends Error {
	public constructor(preparationId: string) {
		super(`Desktop runtime preparation ${preparationId} has a different instance identity`);
		this.name = 'DesktopRuntimePreparationIdentityError';
	}
}

class DesktopRuntimePreparationStateError extends Error {
	public constructor(preparationId: string, expectedState: RuntimePreparationState['kind']) {
		super(`Desktop runtime preparation ${preparationId} is not ${expectedState}`);
		this.name = 'DesktopRuntimePreparationStateError';
	}
}

class DesktopRuntimePreparationSupersededError extends Error {
	public constructor(preparationId: string, baseRevision: number, activeRevision: number) {
		super(
			`Desktop runtime preparation ${preparationId} at revision ${baseRevision.toString()} was superseded by revision ${activeRevision.toString()}`,
		);
		this.name = 'DesktopRuntimePreparationSupersededError';
	}
}

interface RuntimeTransactionIdentity {
	readonly preparationId: string;
	readonly instanceKey: string;
	readonly baseRevision: number;
}

interface CommittedRuntimeTransactionIdentity extends RuntimeTransactionIdentity {
	readonly committedRevision: number;
}

interface PreparedRuntimePreparationState {
	readonly kind: 'prepared';
}

interface CommittedRuntimePreparationState {
	readonly kind: 'committed';
	readonly committedRevision: number;
	readonly previousPlan: LocalAppRuntimePlan | null;
}

type RuntimePreparationState = PreparedRuntimePreparationState | CommittedRuntimePreparationState;

interface RuntimePreparationRecord extends RuntimeTransactionIdentity {
	readonly baseActiveInstanceKey: string | null;
	readonly owner: RendererDocumentOwner;
	readonly plan: LocalAppRuntimePlan;
	readonly watcher: RendererDocumentOwnerWatcher;
	state: RuntimePreparationState;
}

interface ProvisionalRuntimeCommit {
	readonly preparation: RuntimePreparationRecord;
	readonly state: CommittedRuntimePreparationState;
}

interface PendingRuntimePreparation {
	readonly preparationId: string;
	readonly owner: RendererDocumentOwner;
	readonly watcher: RendererDocumentOwnerWatcher;
	readonly controller: AbortController;
}

export class DesktopRuntimeTransactionRegistry {
	private readonly pending = new Map<string, PendingRuntimePreparation>();
	private readonly preparations = new Map<string, RuntimePreparationRecord>();
	private activationRevision = 0;

	public constructor(private readonly rendererDocuments: RendererDocumentOwnerFactory) {}

	public initialize(): void {
		if (this.pending.size !== 0 || this.preparations.size !== 0) {
			throw new Error('Desktop runtime transaction registry cannot initialize with active transactions');
		}
		this.activationRevision = getDesktopLocalAppProtocol().getActivePlan() === null ? 0 : 1;
	}

	public async prepare(event: RendererDocumentIpcEvent, request: unknown): Promise<DesktopRuntimePreparation> {
		const record = readExactPlainRecord({value: request, expectedKeys: PREPARE_REQUEST_KEYS});
		if (record == null) {
			throw new InvalidDesktopRuntimeTransactionRequestError('prepare request');
		}
		const preparationId = readBoundedString(record.preparationId, REQUEST_ID_MAX_LENGTH);
		const instanceKey = readBoundedString(record.instanceKey, INSTANCE_KEY_MAX_LENGTH);
		if (preparationId == null || instanceKey == null) {
			throw new InvalidDesktopRuntimeTransactionRequestError('prepare request');
		}
		if (this.pending.has(preparationId) || this.preparations.has(preparationId)) {
			throw new DesktopRuntimePreparationAlreadyExistsError(preparationId);
		}
		if (this.preparations.size + this.pending.size >= MAX_RUNTIME_PREPARATIONS) {
			throw new DesktopRuntimePreparationCapacityError();
		}
		const owner = this.rendererDocuments.capture(event, 'Desktop runtime config prepare');
		const controller = new AbortController();
		const watcher = owner.watchInvalidation((reason) => {
			controller.abort(reason);
			this.handleOwnerInvalidated(preparationId, owner, reason);
		});
		const pending = {preparationId, owner, watcher, controller};
		this.pending.set(preparationId, pending);
		try {
			const protocol = getDesktopLocalAppProtocol();
			const cached = protocol.findPlanForRoute(instanceKey);
			const signal = AbortSignal.any([controller.signal, protocol.getShutdownSignal()]);
			const plan = cached ?? (await discoverPlanForInstanceKey(instanceKey, signal));
			owner.requireCurrent('Desktop runtime config prepare');
			if (this.pending.get(preparationId) !== pending) {
				throw new DesktopRuntimePreparationNotFoundError(preparationId);
			}
			if (plan.instanceKey !== instanceKey) {
				throw new DesktopRuntimeInstanceMismatchError(instanceKey, plan.instanceKey);
			}
			const preparation: RuntimePreparationRecord = {
				preparationId,
				instanceKey,
				baseRevision: this.activationRevision,
				baseActiveInstanceKey: protocol.getActivePlan()?.instanceKey ?? null,
				owner,
				plan,
				watcher,
				state: {kind: 'prepared'},
			};
			this.pending.delete(preparationId);
			this.preparations.set(preparationId, preparation);
			return buildRuntimePreparation(preparation);
		} catch (error) {
			this.disposePending(pending);
			throw error;
		}
	}

	public commit(event: RendererDocumentIpcEvent, request: unknown): DesktopRuntimeCommit {
		const identity = requireTransactionIdentity(request);
		const preparation = this.requirePreparation(event, identity);
		if (preparation.state.kind !== 'prepared') {
			throw new DesktopRuntimePreparationStateError(preparation.preparationId, 'prepared');
		}
		for (const existing of this.preparations.values()) {
			if (existing !== preparation && existing.state.kind === 'committed') {
				throw new DesktopRuntimeCommitInProgressError(preparation.preparationId);
			}
		}
		if (preparation.baseRevision !== this.activationRevision) {
			throw new DesktopRuntimePreparationSupersededError(
				preparation.preparationId,
				preparation.baseRevision,
				this.activationRevision,
			);
		}
		const previousPlan = getDesktopLocalAppProtocol().getActivePlan();
		if ((previousPlan?.instanceKey ?? null) !== preparation.baseActiveInstanceKey) {
			throw new DesktopRuntimePreparationIdentityError(preparation.preparationId);
		}
		const committedRevision = this.activate(preparation.plan);
		preparation.state = {kind: 'committed', committedRevision, previousPlan};
		return {
			preparationId: preparation.preparationId,
			instanceKey: preparation.instanceKey,
			baseRevision: preparation.baseRevision,
			revision: committedRevision,
		};
	}

	public abort(event: RendererDocumentIpcEvent, request: unknown): DesktopRuntimeAbort {
		const preparationId = requirePreparationId(request);
		const pending = this.pending.get(preparationId);
		if (pending !== undefined) {
			this.requireOwner(pending.owner, event, preparationId);
			this.disposePending(pending);
			return this.abortResult('aborted');
		}
		const preparation = this.preparations.get(preparationId);
		if (preparation === undefined) {
			return this.abortResult('absent');
		}
		this.requireOwner(preparation.owner, event, preparationId);
		if (preparation.state.kind === 'prepared') {
			this.dispose(preparation);
			return this.abortResult('aborted');
		}
		const committed = preparation.state;
		if (committed.committedRevision !== this.activationRevision) {
			this.dispose(preparation);
			throw new DesktopRuntimePreparationSupersededError(
				preparation.preparationId,
				committed.committedRevision,
				this.activationRevision,
			);
		}
		this.restore(committed.previousPlan);
		this.dispose(preparation);
		return this.abortResult('rolled-back');
	}

	public finalize(event: RendererDocumentIpcEvent, request: unknown): DesktopRuntimeFinalization {
		const identity = requireCommittedTransactionIdentity(request);
		const preparation = this.requirePreparation(event, identity);
		this.requireCommitted(preparation, identity.committedRevision);
		this.dispose(preparation);
		return identity;
	}

	public rollback(event: RendererDocumentIpcEvent, request: unknown): DesktopRuntimeRollback {
		const identity = requireCommittedTransactionIdentity(request);
		const preparation = this.requirePreparation(event, identity);
		const committed = this.requireCommitted(preparation, identity.committedRevision);
		if (committed.committedRevision !== this.activationRevision) {
			this.dispose(preparation);
			throw new DesktopRuntimePreparationSupersededError(
				preparation.preparationId,
				committed.committedRevision,
				this.activationRevision,
			);
		}
		const rollback = this.restore(committed.previousPlan);
		this.dispose(preparation);
		return rollback;
	}

	public deactivate(event: RendererDocumentIpcEvent, request: unknown): DesktopRuntimeDeactivation {
		this.rendererDocuments.capture(event, 'Desktop runtime config deactivate');
		const rendererActiveInstanceKey = requireDeactivationInstanceKey(request);
		if (this.pending.size !== 0 || this.preparations.size !== 0) {
			throw new DesktopRuntimeDeactivationInProgressError();
		}
		const protocol = getDesktopLocalAppProtocol();
		const activePlan = protocol.getActivePlan();
		if (activePlan === null || activePlan.instanceKey !== rendererActiveInstanceKey) {
			throw new DesktopRuntimeDeactivationIdentityError();
		}
		const revision = this.nextRevision();
		protocol.deactivateRuntimePlan();
		this.activationRevision = revision;
		return {revision, deactivatedInstanceKey: activePlan.instanceKey};
	}

	public cleanup(): void {
		const provisional = this.findProvisionalCommit(this.preparations.values());
		if (provisional !== null) {
			try {
				this.restore(provisional.state.previousPlan);
			} catch (error) {
				log.error('Failed to roll back a provisional runtime during transaction cleanup', {
					preparationId: provisional.preparation.preparationId,
					error,
				});
			}
		}
		for (const preparation of this.preparations.values()) {
			preparation.watcher.dispose();
		}
		this.preparations.clear();
		for (const preparation of this.pending.values()) {
			preparation.controller.abort();
			preparation.watcher.dispose();
		}
		this.pending.clear();
		this.activationRevision = 0;
	}

	private activate(plan: LocalAppRuntimePlan): number {
		const revision = this.nextRevision();
		getDesktopLocalAppProtocol().activateRuntimePlan(plan);
		this.activationRevision = revision;
		return revision;
	}

	private restore(plan: LocalAppRuntimePlan | null): DesktopRuntimeRollback {
		const revision = this.nextRevision();
		const protocol = getDesktopLocalAppProtocol();
		if (plan === null) {
			protocol.deactivateRuntimePlan();
		} else {
			protocol.activateRuntimePlan(plan);
		}
		this.activationRevision = revision;
		return {revision, activeInstanceKey: plan?.instanceKey ?? null};
	}

	private nextRevision(): number {
		if (
			!Number.isSafeInteger(this.activationRevision) ||
			this.activationRevision < 0 ||
			this.activationRevision >= Number.MAX_SAFE_INTEGER
		) {
			throw new DesktopRuntimeActivationRevisionExhaustedError();
		}
		return this.activationRevision + 1;
	}

	private abortResult(disposition: DesktopRuntimeAbort['disposition']): DesktopRuntimeAbort {
		return {
			disposition,
			revision: this.activationRevision,
			activeInstanceKey: getDesktopLocalAppProtocol().getActivePlan()?.instanceKey ?? null,
		};
	}

	private handleOwnerInvalidated(
		preparationId: string,
		owner: RendererDocumentOwner,
		reason: RendererDocumentInvalidationReason,
	): void {
		const ownedPreparations = [...this.preparations.values()].filter((preparation) =>
			isSameRendererDocument(preparation.owner, owner),
		);
		const provisional = this.findProvisionalCommit(ownedPreparations);
		if (provisional !== null) {
			try {
				this.restore(provisional.state.previousPlan);
			} catch (error) {
				log.error('Failed to roll back a committed runtime after renderer invalidation', {
					preparationId,
					reason,
					error,
				});
			}
		}
		for (const preparation of ownedPreparations) {
			this.dispose(preparation);
		}
		for (const pending of [...this.pending.values()]) {
			if (isSameRendererDocument(pending.owner, owner)) {
				this.disposePending(pending);
			}
		}
	}

	private findProvisionalCommit(preparations: Iterable<RuntimePreparationRecord>): ProvisionalRuntimeCommit | null {
		for (const preparation of preparations) {
			const state = preparation.state;
			if (state.kind === 'committed' && state.committedRevision === this.activationRevision) {
				return {preparation, state};
			}
		}
		return null;
	}

	private requirePreparation(
		event: RendererDocumentIpcEvent,
		identity: RuntimeTransactionIdentity,
	): RuntimePreparationRecord {
		const preparation = this.preparations.get(identity.preparationId);
		if (preparation === undefined) {
			throw new DesktopRuntimePreparationNotFoundError(identity.preparationId);
		}
		this.requireOwner(preparation.owner, event, identity.preparationId);
		if (preparation.instanceKey !== identity.instanceKey || preparation.baseRevision !== identity.baseRevision) {
			throw new DesktopRuntimePreparationIdentityError(identity.preparationId);
		}
		return preparation;
	}

	private requireOwner(owner: RendererDocumentOwner, event: RendererDocumentIpcEvent, preparationId: string): void {
		if (!owner.matchesEvent(event)) {
			throw new DesktopRuntimePreparationOwnershipError(preparationId);
		}
	}

	private requireCommitted(
		preparation: RuntimePreparationRecord,
		committedRevision: number,
	): CommittedRuntimePreparationState {
		const state = preparation.state;
		if (state.kind !== 'committed' || state.committedRevision !== committedRevision) {
			throw new DesktopRuntimePreparationStateError(preparation.preparationId, 'committed');
		}
		return state;
	}

	private dispose(preparation: RuntimePreparationRecord): void {
		if (this.preparations.get(preparation.preparationId) !== preparation) {
			return;
		}
		this.preparations.delete(preparation.preparationId);
		preparation.watcher.dispose();
	}

	private disposePending(preparation: PendingRuntimePreparation): void {
		if (this.pending.get(preparation.preparationId) !== preparation) {
			return;
		}
		this.pending.delete(preparation.preparationId);
		preparation.controller.abort();
		preparation.watcher.dispose();
	}
}

function buildRuntimePreparation(preparation: RuntimePreparationRecord): DesktopRuntimePreparation {
	return {
		...projectDesktopRuntimePlan(preparation.plan),
		preparationId: preparation.preparationId,
		baseRevision: preparation.baseRevision,
		baseActiveInstanceKey: preparation.baseActiveInstanceKey,
	};
}

async function discoverPlanForInstanceKey(instanceKey: string, signal: AbortSignal): Promise<LocalAppRuntimePlan> {
	const plan = await resolveDesktopRuntimePlan({input: instanceKey, signal});
	if (plan.instanceKey !== instanceKey) {
		throw new DesktopRuntimeInstanceMismatchError(instanceKey, plan.instanceKey);
	}
	return plan;
}

function requireTransactionIdentity(request: unknown): RuntimeTransactionIdentity {
	const record = readExactPlainRecord({value: request, expectedKeys: TRANSACTION_REQUEST_KEYS});
	if (record == null) {
		throw new InvalidDesktopRuntimeTransactionRequestError('transaction request');
	}
	const preparationId = readBoundedString(record.preparationId, REQUEST_ID_MAX_LENGTH);
	const instanceKey = readBoundedString(record.instanceKey, INSTANCE_KEY_MAX_LENGTH);
	const baseRevision = readRevision(record.baseRevision, true);
	if (preparationId == null || instanceKey == null || baseRevision == null) {
		throw new InvalidDesktopRuntimeTransactionRequestError('transaction request');
	}
	return {preparationId, instanceKey, baseRevision};
}

function requirePreparationId(request: unknown): string {
	const record = readExactPlainRecord({value: request, expectedKeys: ['preparationId']});
	const preparationId = record == null ? null : readBoundedString(record.preparationId, REQUEST_ID_MAX_LENGTH);
	if (preparationId == null) {
		throw new InvalidDesktopRuntimeTransactionRequestError('abort request');
	}
	return preparationId;
}

function requireCommittedTransactionIdentity(request: unknown): CommittedRuntimeTransactionIdentity {
	const record = readExactPlainRecord({value: request, expectedKeys: COMMITTED_TRANSACTION_REQUEST_KEYS});
	if (record == null) {
		throw new InvalidDesktopRuntimeTransactionRequestError('committed transaction request');
	}
	const preparationId = readBoundedString(record.preparationId, REQUEST_ID_MAX_LENGTH);
	const instanceKey = readBoundedString(record.instanceKey, INSTANCE_KEY_MAX_LENGTH);
	const baseRevision = readRevision(record.baseRevision, true);
	const committedRevision = readRevision(record.committedRevision, false);
	if (preparationId == null || instanceKey == null || baseRevision == null || committedRevision == null) {
		throw new InvalidDesktopRuntimeTransactionRequestError('committed transaction request');
	}
	return {preparationId, instanceKey, baseRevision, committedRevision};
}

function requireDeactivationInstanceKey(request: unknown): string {
	const record = readExactPlainRecord({value: request, expectedKeys: DEACTIVATE_REQUEST_KEYS});
	if (record == null) {
		throw new InvalidDesktopRuntimeTransactionRequestError('deactivate request');
	}
	const instanceKey = readBoundedString(record.rendererActiveInstanceKey, INSTANCE_KEY_MAX_LENGTH);
	if (instanceKey == null) {
		throw new InvalidDesktopRuntimeTransactionRequestError('deactivate request');
	}
	return instanceKey;
}

function readRevision(value: unknown, allowZero: boolean): number | null {
	if (typeof value !== 'number' || !Number.isSafeInteger(value)) {
		return null;
	}
	if (value < (allowZero ? 0 : 1)) {
		return null;
	}
	return value;
}

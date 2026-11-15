// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	OperatorPassRemovalRow,
	OperatorPassUnitRow,
	OperatorPassVisionaryActivationRow,
} from '@app/api/database/types/OperatorPassTypes';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {
	type OperatorPassCertificateService,
	validateOperatorPassCertificateName,
} from '@app/api/operator_pass/OperatorPassCertificateService';
import {isOperatorPassPermanentlyRemoved} from '@app/api/operator_pass/OperatorPassEntitlementService';
import type {OperatorPassHoldingService} from '@app/api/operator_pass/OperatorPassHoldingService';
import {
	OPERATOR_PASS_LABS_INVITE_URL,
	type OperatorPassLabsRoleService,
} from '@app/api/operator_pass/OperatorPassLabsRole';
import {operatorPassHolderLockKey, withOperatorPassLock} from '@app/api/operator_pass/OperatorPassLocks';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {
	normalizeOperatorPassWallChoice,
	type OperatorPassWallChoice,
} from '@app/api/operator_pass/OperatorPassWallPresentation';
import type {OperatorPassWallService} from '@app/api/operator_pass/OperatorPassWallService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {OperatorPassAgeConfirmationRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassAgeConfirmationRequiredError';
import {OperatorPassRemovedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassRemovedError';
import {OperatorPassTermsNotAcceptedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassTermsNotAcceptedError';
import {OperatorPassVisionaryUnavailableError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassVisionaryUnavailableError';
import {
	OPERATOR_PASS_VISIONARY_BOX_VERSION,
	type OperatorPassVisionaryResponse,
	type OperatorPassVisionaryTurnOnRequest,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {OPERATOR_PASS_TERMS_VERSION} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';

const INACTIVE_FLAGS = UserFlags.DELETED | UserFlags.SELF_DELETED | UserFlags.DISABLED | UserFlags.SPAMMER;

export function getOperatorPassVisionaryPassId(userId: UserID): string {
	return `visionary:${userId}:1`;
}

export function isOperatorPassVisionary(user: Pick<User, 'premiumType'>): boolean {
	return user.premiumType === UserPremiumTypes.LIFETIME;
}

function isOperatorPassVisionaryAccountInactive(user: Pick<User, 'flags'>): boolean {
	return (user.flags & INACTIVE_FLAGS) !== 0n;
}

function isOperatorPassVisionaryPendingDeletion(user: Pick<User, 'pendingDeletionAt' | 'deletionStartedAt'>): boolean {
	return user.pendingDeletionAt != null || user.deletionStartedAt != null;
}

type VisionaryPurchaseRecords = Pick<UserRepository, 'findGiftCodesByRedeemer' | 'findPaymentsByUserId'>;

async function findOperatorPassVisionaryPaidAt(
	userRepository: VisionaryPurchaseRecords,
	userId: UserID,
): Promise<Date | null> {
	const [gifts, payments] = await Promise.all([
		userRepository.findGiftCodesByRedeemer(userId),
		userRepository.findPaymentsByUserId(userId),
	]);
	const dates = [
		...gifts
			.filter((code) => code.revokedAt == null && (code.visionarySequenceNumber != null || code.durationQuantity === 0))
			.map((code) => code.redeemedAt),
		...payments
			.filter((payment) => !payment.isGift && /lifetime|visionary/iu.test(payment.productType ?? ''))
			.map((payment) => payment.completedAt),
	].filter((date) => date != null);
	return dates.length > 0 ? new Date(Math.min(...dates.map((date) => date.getTime()))) : null;
}

export async function resolveOperatorPassVisionarySince(
	userRepository: VisionaryPurchaseRecords,
	user: Pick<User, 'id' | 'premiumSince'>,
): Promise<Date | null> {
	return (await findOperatorPassVisionaryPaidAt(userRepository, user.id)) ?? user.premiumSince;
}

export function describeOperatorPassVisionary(params: {
	user: Pick<User, 'premiumType'>;
	visionarySince: Date | null;
	activation: OperatorPassVisionaryActivationRow | null;
	removal: OperatorPassRemovalRow | null;
	labsMember: boolean;
	certificateUrl: string | null;
}): OperatorPassVisionaryResponse | undefined {
	const {user, visionarySince, activation, removal, labsMember, certificateUrl} = params;
	if (activation?.activated_at && (activation.completed_at || activation.ended_at)) {
		return {
			state: activation.ended_at ? 'ended' : 'on',
			reason: null,
			since: (activation.visionary_since ?? activation.activated_at).toISOString(),
			pass_id: activation.pass_id ?? null,
			certificate_number: activation.certificate_number ?? null,
			certificate_url: activation.ended_at ? null : certificateUrl,
			labs_member: labsMember,
			terms_version: activation.terms_version ?? OPERATOR_PASS_TERMS_VERSION,
		};
	}
	const since = activation?.visionary_since ?? visionarySince;
	if (!isOperatorPassVisionary(user) || !since) {
		return undefined;
	}
	const reason: OperatorPassVisionaryResponse['reason'] = isOperatorPassPermanentlyRemoved(removal) ? 'removed' : null;
	return {
		state: reason ? 'unavailable' : 'available',
		reason,
		since: since.toISOString(),
		pass_id: null,
		certificate_number: null,
		certificate_url: null,
		labs_member: labsMember,
		terms_version: OPERATOR_PASS_TERMS_VERSION,
	};
}

export interface OperatorPassVisionaryTurnOnParams {
	user: User;
	request: OperatorPassVisionaryTurnOnRequest;
	boxText: string;
	clientIp: string | null;
	userAgent: string | null;
}

interface OperatorPassVisionaryDependencies {
	repository: OperatorPassRepository;
	userRepository: UserRepository;
	holdingService: OperatorPassHoldingService;
	wallService: OperatorPassWallService;
	roleService: OperatorPassLabsRoleService;
	certificates: OperatorPassCertificateService;
	emailService: IEmailService;
	cacheService: ICacheService;
}

function visionaryLockKey(userId: UserID): string {
	return `operator_pass:visionary_lock:${userId}`;
}

function busyError(): ConflictError {
	return new ConflictError({
		code: APIErrorCodes.CONFLICT,
		message: 'The Operator Pass that comes with Visionary is being turned on in another request',
	});
}

export class OperatorPassVisionaryService {
	constructor(private readonly deps: OperatorPassVisionaryDependencies) {}

	async turnOn(params: OperatorPassVisionaryTurnOnParams): Promise<void> {
		await withOperatorPassLock(
			this.deps.cacheService,
			visionaryLockKey(params.user.id),
			() => this.turnOnLocked(params),
			busyError,
		);
	}

	private async turnOnLocked({
		user: sessionUser,
		request,
		boxText,
		clientIp,
		userAgent,
	}: OperatorPassVisionaryTurnOnParams): Promise<void> {
		const now = operatorPassNow();
		const user = await this.deps.userRepository.findUnique(sessionUser.id);
		if (
			!user?.premiumSince ||
			!isOperatorPassVisionary(user) ||
			isOperatorPassVisionaryAccountInactive(user) ||
			isOperatorPassVisionaryPendingDeletion(user)
		) {
			throw new OperatorPassVisionaryUnavailableError();
		}
		if (isOperatorPassPermanentlyRemoved(await this.deps.repository.findRemoval(user.id))) {
			throw new OperatorPassRemovedError();
		}
		if (
			!request.terms_accepted ||
			request.terms_version !== OPERATOR_PASS_TERMS_VERSION ||
			request.box_text_version !== OPERATOR_PASS_VISIONARY_BOX_VERSION
		) {
			throw new OperatorPassTermsNotAcceptedError();
		}
		if (!request.age_confirmed) {
			throw new OperatorPassAgeConfirmationRequiredError();
		}
		const holderName = await validateOperatorPassCertificateName(request.certificate_name);
		const choice: OperatorPassWallChoice =
			request.wall && !request.wall.opted_out
				? normalizeOperatorPassWallChoice({name: request.wall.name, optOut: false})
				: {name: null, optedOut: true};
		const passId = getOperatorPassVisionaryPassId(user.id);
		const heldPassBefore = (await this.deps.repository.listUnitsByHolder(user.id)).some(
			(unit) => unit.status === 'held' && unit.pass_id !== passId,
		);
		const activation = await this.recordActivation({
			user,
			visionarySince: (await findOperatorPassVisionaryPaidAt(this.deps.userRepository, user.id)) ?? user.premiumSince,
			now,
			boxText,
			clientIp,
			userAgent,
		});
		if (activation.ended_at) {
			throw new OperatorPassVisionaryUnavailableError();
		}
		if (activation.completed_at) {
			await this.deps.roleService.deliver(user, request.labs_greeting ?? null);
			return;
		}
		const visionarySince = activation.visionary_since ?? user.premiumSince;
		const termsVersion = activation.terms_version ?? OPERATOR_PASS_TERMS_VERSION;
		await withOperatorPassLock(
			this.deps.cacheService,
			operatorPassHolderLockKey(user.id),
			() => this.createUnit(user.id, passId, visionarySince, activation.activated_at ?? now),
			busyError,
		);
		const page = await this.deps.certificates.issueVisionaryPage({
			passId,
			holderName,
			visionarySince,
			termsVersion,
		});
		const badgeAlreadyShown = heldPassBefore && !user.hiddenProfileBadges.has('operator_pass');
		const badgeShown = badgeAlreadyShown || request.show_badge;
		if (!badgeAlreadyShown) {
			await this.applyBadgeChoice(user, request.show_badge);
		}
		const keepsWall = await this.deps.wallService.hasChoice(user.id);
		await this.deps.wallService.deliver({
			userId: user.id,
			purchaseId: null,
			choice: keepsWall ? null : choice,
			deliveredAt: now,
			revisionSource: 'settings',
		});
		await this.deps.holdingService.syncHolding(user.id, now);
		const held = await this.deps.repository.listUnitsByHolder(user.id);
		if (!held.some((unit) => unit.pass_id === passId && unit.status === 'held')) {
			throw new Error(`Operator Pass ${passId} is not held by ${user.id} after turning it on`);
		}
		const completed = await this.markCompleted(user.id, page.number, now);
		await this.deps.roleService.deliver(user, request.labs_greeting ?? null);
		await this.deps.holdingService.dispatchPassesUpdate([user.id]);
		if (completed) {
			await this.sendTurnedOnEmail(user, passId, termsVersion, badgeShown);
		}
	}

	private async markCompleted(userId: UserID, certificateNumber: number, now: Date): Promise<boolean> {
		for (let attempt = 0; attempt < 3; attempt++) {
			const current = await this.deps.repository.findVisionaryActivation(userId);
			if (!current?.activated_at || current.ended_at) {
				throw new OperatorPassVisionaryUnavailableError();
			}
			if (current.completed_at) {
				return false;
			}
			const updated = await this.deps.repository.updateVisionaryActivation(current, {
				certificate_number: Db.set(current.certificate_number ?? certificateNumber),
				completed_at: Db.set(now),
			});
			if (updated) {
				return true;
			}
		}
		throw busyError();
	}

	private async recordActivation(params: {
		user: User;
		visionarySince: Date;
		now: Date;
		boxText: string;
		clientIp: string | null;
		userAgent: string | null;
	}): Promise<OperatorPassVisionaryActivationRow> {
		const {user, now} = params;
		const patch = {
			visionary_since: Db.set(params.visionarySince),
			visionary_sequence: Db.set(user.premiumLifetimeSequence),
			activated_at: Db.set(now),
			terms_version: Db.set(OPERATOR_PASS_TERMS_VERSION),
			terms_box_text: Db.set(params.boxText),
			age_confirmed_at: Db.set(now),
			consent_ip: Db.set(params.clientIp),
			consent_user_agent: Db.set(params.userAgent),
			pass_id: Db.set(getOperatorPassVisionaryPassId(user.id)),
		};
		for (let attempt = 0; attempt < 3; attempt++) {
			const current = await this.ensureActivationRow(user.id);
			if (current.activated_at) {
				return current;
			}
			const updated = await this.deps.repository.updateVisionaryActivation(current, patch);
			if (updated) {
				return updated;
			}
		}
		throw busyError();
	}

	private async ensureActivationRow(userId: UserID): Promise<OperatorPassVisionaryActivationRow> {
		const existing = await this.deps.repository.findVisionaryActivation(userId);
		if (existing) {
			return existing;
		}
		await this.deps.repository.createVisionaryActivation(emptyOperatorPassVisionaryActivation(userId));
		const created = await this.deps.repository.findVisionaryActivation(userId);
		if (!created) {
			throw new Error(`Operator Pass Visionary activation for ${userId} was not written`);
		}
		return created;
	}

	private async createUnit(userId: UserID, passId: string, visionarySince: Date, activatedAt: Date): Promise<void> {
		const unit: OperatorPassUnitRow = {
			pass_id: passId,
			purchase_id: null,
			buyer_id: null,
			holder_id: userId,
			source: 'visionary',
			status: 'held',
			transferable: false,
			delivered_at: visionarySince,
			first_delivered_at: visionarySince,
			paused_at: null,
			ended_at: null,
			end_reason: null,
			withdrawal_country: null,
			withdrawal_days: null,
			version: 1,
		};
		if (!(await this.deps.repository.createUnit(unit))) {
			const existing = await this.deps.repository.findUnit(passId);
			if (existing?.status !== 'held' || existing.holder_id !== userId) {
				throw new OperatorPassVisionaryUnavailableError();
			}
			await this.deps.repository.indexUnitHolder(passId, userId);
		}
		await this.deps.holdingService.appendVisionaryEvents(passId, userId, activatedAt);
	}

	private async applyBadgeChoice(user: User, show: boolean): Promise<void> {
		if (user.hiddenProfileBadges.has('operator_pass') !== show) {
			return;
		}
		const next = new Set<string>(user.hiddenProfileBadges);
		if (show) {
			next.delete('operator_pass');
		} else {
			next.add('operator_pass');
		}
		await this.deps.userRepository.patchUpsert(
			user.id,
			{hidden_profile_badges: next.size > 0 ? next : null},
			user.toRow(),
		);
	}

	private async sendTurnedOnEmail(
		user: User,
		passId: string,
		termsVersion: string,
		badgeShown: boolean,
	): Promise<void> {
		try {
			if (!user.email || user.emailBounced) {
				return;
			}
			const file = await this.deps.certificates.loadVisionaryFile(passId);
			if (!file) {
				return;
			}
			const wall = await this.deps.wallService.getEntry(user.id);
			await this.deps.emailService.send(
				'operator_pass_visionary_turned_on',
				user.email,
				{
					badgeShown,
					wallName: wall && !wall.opted_out ? (wall.name ?? null) : null,
					labsMember: await this.deps.roleService.isLabsMember(user.id),
					labsInviteUrl: OPERATOR_PASS_LABS_INVITE_URL,
					termsVersion,
					certificate: {filename: file.filename, content: Buffer.from(file.bytes), contentType: 'application/pdf'},
					timeZone: user.timezone,
				},
				user.locale,
			);
		} catch (error) {
			Logger.warn({userId: user.id, error}, 'Failed to send the Operator Pass email after a Visionary turned it on');
		}
	}

	async endIfVisionaryEnded(userId: UserID): Promise<boolean> {
		const activation = await this.deps.repository.findVisionaryActivation(userId);
		if (!activation?.activated_at || activation.ended_at) {
			return false;
		}
		const user = await this.deps.userRepository.findUnique(userId);
		if (user && isOperatorPassVisionary(user)) {
			return false;
		}
		return withOperatorPassLock(
			this.deps.cacheService,
			visionaryLockKey(userId),
			async () => {
				const current = await this.deps.repository.findVisionaryActivation(userId);
				if (!current?.activated_at || current.ended_at) {
					return false;
				}
				await this.deps.holdingService.endVisionaryUnit(userId, getOperatorPassVisionaryPassId(userId));
				await this.markEnded(userId, operatorPassNow());
				await this.deps.holdingService.dispatchPassesUpdate([userId]);
				return true;
			},
			busyError,
		);
	}

	private async markEnded(userId: UserID, now: Date): Promise<void> {
		for (let attempt = 0; attempt < 3; attempt++) {
			const current = await this.deps.repository.findVisionaryActivation(userId);
			if (!current || current.ended_at) {
				return;
			}
			const updated = await this.deps.repository.updateVisionaryActivation(current, {
				ended_at: Db.set(now),
				end_reason: Db.set('visionary_ended'),
			});
			if (updated) {
				return;
			}
		}
		throw busyError();
	}

	async endAllEndedVisionaries(pageSize: number): Promise<{ended: number; failed: number}> {
		const summary = {ended: 0, failed: 0};
		let pageState: string | null = null;
		do {
			const page = await this.deps.repository.scanVisionaryActivations(pageSize, pageState);
			for (const row of page.rows) {
				if (!row.activated_at || row.ended_at) {
					continue;
				}
				try {
					if (await this.endIfVisionaryEnded(row.user_id)) {
						summary.ended++;
					}
				} catch (error) {
					summary.failed++;
					Logger.error({userId: row.user_id, error}, 'Failed to end the Operator Pass of a Visionary that ended');
				}
			}
			pageState = page.pageState;
		} while (pageState);
		return summary;
	}
}

function emptyOperatorPassVisionaryActivation(userId: UserID): OperatorPassVisionaryActivationRow {
	return {
		user_id: userId,
		visionary_since: null,
		visionary_sequence: null,
		activated_at: null,
		terms_version: null,
		terms_box_text: null,
		age_confirmed_at: null,
		consent_ip: null,
		consent_user_agent: null,
		pass_id: null,
		certificate_number: null,
		completed_at: null,
		ended_at: null,
		end_reason: null,
		notice_emailed_at: null,
		notice_skip_reason: null,
		notice_dm_sent_at: null,
		version: 1,
	};
}

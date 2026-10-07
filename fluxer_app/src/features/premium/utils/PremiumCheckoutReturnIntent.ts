// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import PremiumState from '@app/features/premium/state/PremiumState';
import Users from '@app/features/user/state/Users';

const STORAGE_KEY = 'PremiumCheckoutReturnIntent';
const INTENT_TTL_MS = 30 * 60 * 1000;

export type PremiumCheckoutReturnSource = 'plutonium';

interface PremiumCheckoutReturnIntent {
	id: string;
	source: PremiumCheckoutReturnSource;
	userId: string | null;
	createdAt: number;
	expiresAt: number;
	startedPremium: boolean;
}

function getStorage(): Storage | null {
	return getProtectedSessionStorage();
}

function generateIntentId(): string {
	return `premium-checkout-${Date.now()}-${Math.random().toString(36).slice(2)}`;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function parsePremiumCheckoutReturnIntent(raw: string): PremiumCheckoutReturnIntent | null {
	const parsed: unknown = JSON.parse(raw);
	if (!isRecord(parsed)) {
		return null;
	}
	if (
		typeof parsed.id !== 'string' ||
		parsed.source !== 'plutonium' ||
		(typeof parsed.userId !== 'string' && parsed.userId !== null) ||
		typeof parsed.createdAt !== 'number' ||
		typeof parsed.expiresAt !== 'number' ||
		typeof parsed.startedPremium !== 'boolean'
	) {
		return null;
	}
	return {
		id: parsed.id,
		source: parsed.source,
		userId: parsed.userId,
		createdAt: parsed.createdAt,
		expiresAt: parsed.expiresAt,
		startedPremium: parsed.startedPremium,
	};
}

export function getCurrentPremiumActive(): boolean {
	const currentUser = Users.currentUser;
	if (!currentUser) return false;
	const premiumState = PremiumState.loadedForUserId === currentUser.id ? PremiumState.state : null;
	return premiumState?.effective.is_premium ?? currentUser.isPremium();
}

function readIntent(): PremiumCheckoutReturnIntent | null {
	const storage = getStorage();
	if (!storage) return null;
	const raw = storage.getItem(STORAGE_KEY);
	if (!raw) return null;
	try {
		const parsed = parsePremiumCheckoutReturnIntent(raw);
		if (!parsed) {
			storage.removeItem(STORAGE_KEY);
			return null;
		}
		if (Date.now() > parsed.expiresAt) {
			storage.removeItem(STORAGE_KEY);
			return null;
		}
		return parsed as PremiumCheckoutReturnIntent;
	} catch {
		storage.removeItem(STORAGE_KEY);
		return null;
	}
}

export function recordPremiumCheckoutReturnIntent(source: PremiumCheckoutReturnSource): void {
	const storage = getStorage();
	if (!storage) return;
	const currentUser = Users.currentUser;
	const startedPremium = getCurrentPremiumActive();
	if (startedPremium) {
		storage.removeItem(STORAGE_KEY);
		return;
	}
	const now = Date.now();
	const intent: PremiumCheckoutReturnIntent = {
		id: generateIntentId(),
		source,
		userId: currentUser?.id ?? null,
		createdAt: now,
		expiresAt: now + INTENT_TTL_MS,
		startedPremium,
	};
	storage.setItem(STORAGE_KEY, JSON.stringify(intent));
}

export function getPendingPremiumCheckoutReturnIntent(): PremiumCheckoutReturnIntent | null {
	const intent = readIntent();
	if (!intent || intent.startedPremium) return null;
	const currentUserId = Users.currentUser?.id ?? null;
	if (intent.userId && intent.userId !== currentUserId) {
		clearPremiumCheckoutReturnIntent();
		return null;
	}
	return intent;
}

export function consumeCompletedPremiumCheckoutReturnIntent(): PremiumCheckoutReturnIntent | null {
	const intent = getPendingPremiumCheckoutReturnIntent();
	if (!intent || !getCurrentPremiumActive()) return null;
	clearPremiumCheckoutReturnIntent();
	return intent;
}

function clearPremiumCheckoutReturnIntent(): void {
	getStorage()?.removeItem(STORAGE_KEY);
}

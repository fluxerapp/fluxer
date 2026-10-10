// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {USERNAME_MODE_DISCRIMINATOR} from '@app/api/user/UserTag';
import {DELETED_USER_USERNAME, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {ms} from 'itty-time';

type UsernameLookup = Pick<UserRepository, 'findUsersByUsername'>;

interface UniqueUsernameDeps {
	users: UsernameLookup;
	cache: ICacheService;
}

export interface UsernameReservation {
	readonly username: string;
	release(): Promise<void>;
}

export interface ParsedLoginHandle {
	username: string;
	discriminator: number | null;
}

const USERNAME_MAX_LENGTH = 32;
const RESERVATION_TTL_SECONDS = 15;
const RESERVATION_WAIT_MS = ms('5 seconds');
const RESERVATION_RETRY_MS = 50;
const DERIVE_ATTEMPTS = 20;
const LOGIN_HANDLE_REGEX = /^([a-zA-Z0-9_]{1,32})(?:#(\d{1,4}))?$/;

function usernameTakenError(): InputValidationError {
	return InputValidationError.fromCode('username', ValidationErrorCodes.USERNAME_ALREADY_TAKEN);
}

export function assertNoDiscriminatorChange(requested: number | undefined, current: number): void {
	if (requested === undefined || requested === current || requested === USERNAME_MODE_DISCRIMINATOR) return;
	throw InputValidationError.fromCode('discriminator', ValidationErrorCodes.DISCRIMINATOR_NOT_SUPPORTED_ON_INSTANCE);
}

export function parseLoginHandle(input: string): ParsedLoginHandle | null {
	const match = LOGIN_HANDLE_REGEX.exec(input.trim());
	if (!match) return null;
	return {
		username: match[1]!,
		discriminator: match[2] === undefined ? null : Number.parseInt(match[2], 10),
	};
}

function isPerson(user: User): boolean {
	return !user.isBot && (user.flags & UserFlags.DELETED) === 0n;
}

async function findUsernameHolders(users: UsernameLookup, username: string): Promise<Array<User>> {
	const usernameLower = username.toLowerCase();
	const holders = new Map<UserID, User>();
	for (const user of await users.findUsersByUsername(username)) {
		if (user.username.toLowerCase() === usernameLower) holders.set(user.id, user);
	}
	return [...holders.values()];
}

async function findPeopleByUsername(users: UsernameLookup, username: string): Promise<Array<User>> {
	return (await findUsernameHolders(users, username)).filter(isPerson);
}

export async function findPersonByLoginHandle(users: UsernameLookup, handle: ParsedLoginHandle): Promise<User | null> {
	const people = await findPeopleByUsername(users, handle.username);
	const matches =
		handle.discriminator === null ? people : people.filter((user) => user.discriminator === handle.discriminator);
	return matches.length === 1 ? matches[0]! : null;
}

export async function isUsernameTaken(
	users: UsernameLookup,
	username: string,
	exceptUserId?: UserID,
): Promise<boolean> {
	if (username.toLowerCase() === DELETED_USER_USERNAME.toLowerCase()) return true;
	const holders = await findUsernameHolders(users, username);
	return holders.some(
		(user) => user.id !== exceptUserId && (isPerson(user) || user.discriminator === USERNAME_MODE_DISCRIMINATOR),
	);
}

async function acquireReservationLock(cache: ICacheService, key: string): Promise<string | null> {
	const deadline = Date.now() + RESERVATION_WAIT_MS;
	while (Date.now() < deadline) {
		const token = await cache.acquireLock(key, RESERVATION_TTL_SECONDS);
		if (token) return token;
		await new Promise((resolve) => setTimeout(resolve, RESERVATION_RETRY_MS));
	}
	return null;
}

export async function reserveUsername(
	deps: UniqueUsernameDeps,
	username: string,
	exceptUserId?: UserID,
): Promise<UsernameReservation> {
	const key = `unique-username:${username.toLowerCase()}`;
	const token = await acquireReservationLock(deps.cache, key);
	if (!token) throw usernameTakenError();
	const release = async () => {
		await deps.cache.releaseLock(key, token);
	};
	try {
		if (await isUsernameTaken(deps.users, username, exceptUserId)) throw usernameTakenError();
	} catch (error) {
		await release();
		throw error;
	}
	return {username, release};
}

function withNumericSuffix(base: string, suffix: number): string {
	const suffixText = String(suffix);
	return `${base.slice(0, USERNAME_MAX_LENGTH - suffixText.length)}${suffixText}`;
}

export async function deriveAvailableUsername(users: UsernameLookup, base: string): Promise<string> {
	for (let attempt = 0; attempt < DERIVE_ATTEMPTS; attempt++) {
		const candidate = attempt === 0 ? base : withNumericSuffix(base, randomInt(1, 10000));
		if (!(await isUsernameTaken(users, candidate))) return candidate;
	}
	throw usernameTakenError();
}

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

export abstract class ICacheService {
	abstract get<T>(key: string): Promise<T | null>;
	abstract set<T>(key: string, value: T, ttlSeconds?: number): Promise<void>;
	abstract delete(key: string): Promise<void>;
	abstract getAndDelete<T>(key: string): Promise<T | null>;
	abstract exists(key: string): Promise<boolean>;
	abstract expire(key: string, ttlSeconds: number): Promise<void>;
	abstract ttl(key: string): Promise<number>;
	abstract mget<T>(keys: Array<string>): Promise<Array<T | null>>;
	abstract mset<T>(entries: Array<{key: string; value: T; ttlSeconds?: number}>): Promise<void>;
	abstract deletePattern(pattern: string): Promise<number>;
	abstract acquireLock(key: string, ttlSeconds: number): Promise<string | null>;
	abstract releaseLock(key: string, token: string): Promise<boolean>;
	abstract getAndRenewTtl<T>(key: string, newTtlSeconds: number): Promise<T | null>;
	abstract publish(channel: string, message: string): Promise<void>;
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import argon2 from 'argon2';

const TEST_ARGON2_OPTIONS: argon2.Options = {
	memoryCost: 1024,
	timeCost: 1,
	parallelism: 1,
};

export async function hashPassword(password: string): Promise<string> {
	const options = Config.dev.testModeEnabled ? TEST_ARGON2_OPTIONS : undefined;
	return argon2.hash(password, options);
}

export async function verifyPassword({
	password,
	passwordHash,
}: {
	password: string;
	passwordHash: string;
}): Promise<boolean> {
	return argon2.verify(passwordHash, password);
}

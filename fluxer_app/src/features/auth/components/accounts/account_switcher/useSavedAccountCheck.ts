// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountAccess from '@app/features/auth/state/AccountAccess';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import type {Account} from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useEffect, useRef} from 'react';

const logger = new Logger('SavedAccountCheck');
const SAVED_ACCOUNT_RECHECK_INTERVAL_MS = 5 * 60_000;

function checkSignature(accounts: ReadonlyArray<Account>): string {
	return accounts.map((account) => `${getAccountKey(account)}:${account.isValid ? 1 : 0}`).join('|');
}

export function useSavedAccountCheck(accounts: ReadonlyArray<Account>, currentAccountKey: string | null): void {
	const accountsRef = useRef(accounts);
	accountsRef.current = accounts;
	const signature = checkSignature(accounts);
	useEffect(() => {
		if (signature.length === 0) {
			return;
		}
		const checkSavedAccounts = () => {
			for (const account of accountsRef.current) {
				const accountKey = getAccountKey(account);
				if (account.isValid === false || accountKey === currentAccountKey) {
					continue;
				}
				void AccountAccess.ensureAccountChecked(accountKey, {maxAgeMs: SAVED_ACCOUNT_RECHECK_INTERVAL_MS}).catch(
					(error: unknown) => {
						logger.warn(`Could not check saved account ${accountKey}`, error);
					},
				);
			}
		};
		checkSavedAccounts();
		window.addEventListener('focus', checkSavedAccounts);
		return () => window.removeEventListener('focus', checkSavedAccounts);
	}, [signature, currentAccountKey]);
}

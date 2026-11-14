// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import {OtherAccountsInboxState} from '@app/features/inbox/state/OtherAccountsInboxState';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';

export default new OtherAccountsInboxState(AccountEngine, AppStorage, AppStorageKey.UI_INBOX_OTHER_ACCOUNTS_COLLAPSED);

// SPDX-License-Identifier: AGPL-3.0-or-later

import {TranslationSafeMessage} from '@app/features/i18n/components/TranslationSafeMessage';
import type {I18n} from '@lingui/core';
import {I18nProvider} from '@lingui/react';
import type {ReactNode} from 'react';

export function AppI18nProvider({i18n, children}: {i18n: I18n; children?: ReactNode}): ReactNode {
	return (
		<I18nProvider i18n={i18n} defaultComponent={TranslationSafeMessage}>
			{children}
		</I18nProvider>
	);
}

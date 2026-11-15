// SPDX-License-Identifier: AGPL-3.0-or-later

import {TranslationSafeMessage} from '@app/features/i18n/components/TranslationSafeMessage';
import {getLocaleDirection} from '@app/features/i18n/utils/LocaleDirection';
import {DirectionProvider} from '@base-ui/react/direction-provider';
import type {I18n} from '@lingui/core';
import {I18nProvider, useLingui} from '@lingui/react';
import type {ReactNode} from 'react';

function LocaleDirectionProvider({children}: {children?: ReactNode}): ReactNode {
	const {i18n} = useLingui();
	return (
		<DirectionProvider
			direction={getLocaleDirection(i18n.locale)}
			data-flx="i18n.app-i18n-provider.locale-direction-provider.direction-provider"
		>
			{children}
		</DirectionProvider>
	);
}

export function AppI18nProvider({i18n, children}: {i18n: I18n; children?: ReactNode}): ReactNode {
	return (
		<I18nProvider i18n={i18n} defaultComponent={TranslationSafeMessage}>
			<LocaleDirectionProvider data-flx="i18n.app-i18n-provider.locale-direction-provider">
				{children}
			</LocaleDirectionProvider>
		</I18nProvider>
	);
}

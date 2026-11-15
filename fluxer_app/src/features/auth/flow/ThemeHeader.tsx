// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuthPageHeader} from '@app/features/auth/flow/AuthPageHeader';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PaletteIcon} from '@phosphor-icons/react';

const YOU_VE_GOT_CSS_DESCRIPTOR = msg({
	message: "You've got CSS!",
	comment: 'Short label in the theme login and register pages. Keep it concise. Keep the tone plain and specific.',
});
const SHARED_THEME_DESCRIPTOR = msg({
	message: 'Shared theme',
	comment:
		'Button or menu action label in the theme login and register pages. Keep it concise. Keep the tone plain and specific.',
});
export function ThemeHeader({prefix, sso = false}: {prefix: string; sso?: boolean}) {
	const {i18n} = useLingui();
	let iconSpotSuffix = 'div';
	if (sso) {
		iconSpotSuffix = 'div.sso';
	} else if (prefix.includes('register')) {
		iconSpotSuffix = 'div--2';
	}
	return (
		<AuthPageHeader
			icon={
				<div className={sharedStyles.themeIconSpot} data-flx={`${prefix}.${iconSpotSuffix}`}>
					<PaletteIcon
						className={sharedStyles.themeIcon}
						weight="fill"
						data-flx={`${prefix}.palette-icon${sso ? '.sso' : ''}`}
					/>
				</div>
			}
			title={i18n._(YOU_VE_GOT_CSS_DESCRIPTOR)}
			subtitle={i18n._(SHARED_THEME_DESCRIPTOR)}
			data-flx={`${prefix}.auth-page-header${sso ? '.sso' : ''}`}
		/>
	);
}

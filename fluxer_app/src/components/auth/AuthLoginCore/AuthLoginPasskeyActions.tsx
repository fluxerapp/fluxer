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

import {Trans} from '@lingui/react/macro';
import {BrowserIcon, KeyIcon} from '@phosphor-icons/react';
import {Button} from '~/components/uikit/Button/Button';

export type AuthLoginDividerClasses = {
	divider: string;
	dividerLine: string;
	dividerText: string;
};

export function AuthLoginDivider({
	classes,
	label = <Trans>OR</Trans>,
}: {
	classes: AuthLoginDividerClasses;
	label?: React.ReactNode;
}) {
	return (
		<div className={classes.divider}>
			<div className={classes.dividerLine} />
			<span className={classes.dividerText}>{label}</span>
			<div className={classes.dividerLine} />
		</div>
	);
}

export type AuthPasskeyClasses = {
	wrapper?: string;
};

type Props = {
	classes?: AuthPasskeyClasses;

	disabled: boolean;

	onPasskeyLogin: () => void;
	showBrowserOption: boolean;
	onBrowserLogin?: () => void;

	primaryLabel?: React.ReactNode;
	browserLabel?: React.ReactNode;
};

export default function AuthLoginPasskeyActions({
	classes,
	disabled,
	onPasskeyLogin,
	showBrowserOption,
	onBrowserLogin,
	primaryLabel = <Trans>Log in with a passkey</Trans>,
	browserLabel = <Trans>Log in via browser</Trans>,
}: Props) {
	return (
		<div className={classes?.wrapper}>
			<Button
				type="button"
				fitContainer
				variant="secondary"
				onClick={onPasskeyLogin}
				disabled={disabled}
				leftIcon={<KeyIcon size={16} />}
			>
				{primaryLabel}
			</Button>

			{showBrowserOption && onBrowserLogin ? (
				<Button
					type="button"
					fitContainer
					variant="secondary"
					onClick={onBrowserLogin}
					disabled={disabled}
					leftIcon={<BrowserIcon size={16} />}
				>
					{browserLabel}
				</Button>
			) : null}
		</div>
	);
}

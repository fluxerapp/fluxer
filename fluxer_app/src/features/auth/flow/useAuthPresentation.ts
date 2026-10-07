// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuthCardVariant, AuthLayoutContentMode, useAuthLayoutContext} from '@app/features/auth/state/AuthLayoutContext';
import {useLayoutEffect} from 'react';

interface AuthPresentationOptions {
	contentMode?: AuthLayoutContentMode;
	variant?: AuthCardVariant;
	enabled?: boolean;
}

export function useAuthPresentation({
	contentMode = AuthLayoutContentMode.CARD,
	variant = AuthCardVariant.DEFAULT,
	enabled = true,
}: AuthPresentationOptions): void {
	const {setCardVariant, setContentMode} = useAuthLayoutContext();
	useLayoutEffect(() => {
		if (!enabled) return;
		setContentMode(contentMode);
		setCardVariant(variant);
	}, [contentMode, enabled, setCardVariant, setContentMode, variant]);
}

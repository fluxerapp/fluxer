// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import React, {useContext} from 'react';

export const AuthCardVariant = Object.freeze({
	DEFAULT: 'default',
	STANDARD: 'standard',
	COMPACT: 'compact',
	WIDE: 'wide',
} as const);

export type AuthCardVariant = (typeof AuthCardVariant)[keyof typeof AuthCardVariant];

export const AuthLayoutContentMode = Object.freeze({
	CARD: 'card',
	FULL: 'full',
} as const);

export type AuthLayoutContentMode = (typeof AuthLayoutContentMode)[keyof typeof AuthLayoutContentMode];

export interface AuthLayoutContextType {
	setSplashUrl: (url: string | null) => void;
	setCardVariant: React.Dispatch<React.SetStateAction<AuthCardVariant>>;
	setContentMode: React.Dispatch<React.SetStateAction<AuthLayoutContentMode>>;
	setSplashCardAlignment: React.Dispatch<React.SetStateAction<GuildSplashCardAlignmentValue>>;
}

export const AuthLayoutContext = React.createContext<AuthLayoutContextType | null>(null);
export const useAuthLayoutContext = () => {
	const context = useContext(AuthLayoutContext);
	if (!context) {
		throw new Error('useAuthLayoutContext must be used within AuthLayoutProvider');
	}
	return context;
};

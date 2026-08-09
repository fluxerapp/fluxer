// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_NAME, PRODUCT_NAME, SPLASH_IRC_SERVER} from '@app/features/app/config/I18nDisplayConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

const LOADING_DESCRIPTOR = msg({
	message: 'Loading {productName}.',
	comment: 'Neutral splash-screen loading fallback for non-English locales. Do not imply that rotating quotes exist.',
});

export interface SplashQuote {
	readonly text: string;
	readonly source: string;
}

const isEnglishLocale = (locale: string | null | undefined): boolean => locale?.toLowerCase().startsWith('en') ?? false;

export function useSplashQuotes(): ReadonlyArray<SplashQuote> {
	const {i18n} = useLingui();
	return useMemo(() => {
		if (!isEnglishLocale(i18n.locale)) {
			return [
				{
					text: i18n._(LOADING_DESCRIPTOR, {productName: PRODUCT_NAME}),
					source: PRODUCT_NAME,
				},
			];
		}
		const quotes: Array<SplashQuote> = [
			{text: "Aren't you supposed to be sleeping?", source: 'kitkatz'},
			{text: 'After careful consideration, we have decided we did, in fact, need loading lines.', source: 'vky'},
			{text: "There is no cloud... only other people's computers.", source: 'Siliconic'},
			{
				text: `Did you know that ${PRODUCT_NAME} spelled backwards makes no sense? Why would you do that?`,
				source: 'jackzie',
			},
			{text: 'Have you considered touching grass while you wait?', source: 'Lilith'},
			{
				text: `Unfortunately, it doesn't run on regular unleaded gasoline. It requires something with a little more kick. ${PREMIUM_PRODUCT_NAME}.`,
				source: 'Cookie',
			},
			{text: `9/10 dentists recommend ${PRODUCT_NAME}.`, source: 'Hugopuntocom'},
			{text: 'Is this thing on?', source: 'Pufty'},
			{text: 'Now supports sending messages over the Internet!', source: '99'},
			{text: 'Flux capacitors use 99.99% less water than your typical AI data center!', source: 'snappyapple632'},
			{text: 'No soldering skills required!', source: '99'},
			{text: 'Removed Herobrine.', source: 'Manfre'},
			{text: 'Please enjoy this loading quote while you wait.', source: 'illspirit'},
			{
				text: `${PRODUCT_NAME} is 100% powered by flux capacitors. And furries. We don't forget about the furries.`,
				source: 'jb',
			},
			{text: 'Unable to decrypt loading quote.', source: 'SteveLinkNoah'},
			{text: "Remember to hydrate! Or don't, I'm not the boss of you.", source: 'jb'},
			{text: 'Congratulations! This is the special rare loading screen message!', source: 'jb'},
			{text: 'Nu \u00e4r det dags f\u00f6r en fika.', source: 'Chip'},
			{text: `Is ${PRODUCT_NAME} loading? Why are you asking me? I'm just a loading screen!`, source: 'Caus'},
			{text: 'Welcome back to 2015!', source: 'Pan'},
			{text: `Connecting to ${SPLASH_IRC_SERVER}...`, source: 'viriona'},
			{text: "Wait, hold on, I'm not ready yet!", source: 'Vee'},
			{text: `Feel that? That's ${PRODUCT_NAME}.`, source: 'TheFastestBoy'},
			{text: `The real ${PRODUCT_NAME} was the friends we made along the way.`, source: 'viriona'},
			{
				text: `Is ${PRODUCT_NAME} an instrument? No, ${PRODUCT_NAME} is a communication platform.`,
				source: 'Comfy_Deer',
			},
			{text: "Hi! I'm a loading screen!", source: 'jackzie'},
			{text: `3 Billion Devices Run ${PRODUCT_NAME}.`, source: 'jackzie'},
			{text: 'To connect or not to connect, that is the question.', source: 'Tea'},
			{text: `Please do not the ${PRODUCT_NAME}.`, source: 'Chris'},
			{text: 'I am the one who fluxes.', source: 'Anonymous'},
			{text: `Installing ${PRODUCT_NAME} on the nearest smart fridge...`, source: 'TyphonBagrid'},
			{text: 'Ya like Jazz?', source: 'catnaros'},
			{text: "If it smells like chicken, you're holding the soldering iron wrong.", source: 'viriona'},
			{text: 'Everything is in flux.', source: 'Alyx'},
			{text: 'Happy new year 1999!', source: '99'},
			{text: 'Heating up the engine.', source: 'Ray'},
			{text: 'Pong!', source: 'Cookie'},
			{text: "Every day I'm fluxin' it.", source: 'Liminalis'},
			{text: 'I am the word', source: 'Bird'},
			{text: 'Back in my day, we had Skype... and Fluxer.', source: 'Stormzady'},
		];
		quotes.push({
			text: `Your odds of landing on this particular loading quote were 1 in ${String(quotes.length + 1)}.`,
			source: 'vky',
		});
		return quotes;
	}, [i18n.locale, i18n]);
}

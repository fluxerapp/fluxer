// SPDX-License-Identifier: AGPL-3.0-or-later

export interface WelcomeRotationEntry {
	readonly code: string;
	readonly text: string;
}

export interface WelcomeRotationState {
	readonly order: ReadonlyArray<number>;
	readonly position: number;
}

type RandomSource = () => number;

export const WELCOME_ROTATION: ReadonlyArray<WelcomeRotationEntry> = [
	{code: 'ar', text: 'مرحبا'},
	{code: 'bg', text: 'Добре дошли'},
	{code: 'cs', text: 'Vítejte'},
	{code: 'da', text: 'Velkommen'},
	{code: 'de', text: 'Willkommen'},
	{code: 'el', text: 'Καλώς ήρθατε'},
	{code: 'en-GB', text: 'Welcome'},
	{code: 'en-US', text: 'Welcome'},
	{code: 'es-ES', text: 'Bienvenido'},
	{code: 'es-419', text: 'Bienvenido'},
	{code: 'fi', text: 'Tervetuloa'},
	{code: 'fr', text: 'Bienvenue'},
	{code: 'he', text: 'ברוכים הבאים'},
	{code: 'hi', text: 'स्वागत है'},
	{code: 'hr', text: 'Dobrodošli'},
	{code: 'hu', text: 'Üdvözöljük'},
	{code: 'id', text: 'Selamat datang'},
	{code: 'it', text: 'Benvenuto'},
	{code: 'ja', text: 'ようこそ'},
	{code: 'ko', text: '환영합니다'},
	{code: 'lt', text: 'Sveiki'},
	{code: 'nl', text: 'Welkom'},
	{code: 'no', text: 'Velkommen'},
	{code: 'pl', text: 'Witaj'},
	{code: 'pt-BR', text: 'Boas-vindas'},
	{code: 'ro', text: 'Bine ați venit'},
	{code: 'ru', text: 'Добро пожаловать'},
	{code: 'sv-SE', text: 'Välkommen'},
	{code: 'th', text: 'ยินดีต้อนรับ'},
	{code: 'tr', text: 'Hoş geldiniz'},
	{code: 'uk', text: 'Ласкаво просимо'},
	{code: 'vi', text: 'Chào mừng'},
	{code: 'zh-CN', text: '欢迎'},
	{code: 'zh-TW', text: '歡迎'},
];

const DEFAULT_WELCOME_LOCALE = 'en-US';

interface WelcomeTextGroup {
	readonly textKey: string;
	readonly indexes: ReadonlyArray<number>;
}

function normalizeWelcomeText(text: string): string {
	return text.normalize('NFKC').trim().replace(/\s+/g, ' ').toLocaleLowerCase();
}

function randomWelcomeIndex(maxExclusive: number, random: RandomSource): number {
	if (maxExclusive <= 1) return 0;
	return Math.floor(random() * maxExclusive);
}

function shuffleWelcomeItems<T>(items: Array<T>, random: RandomSource): Array<T> {
	for (let index = items.length - 1; index > 0; index -= 1) {
		const swapIndex = randomWelcomeIndex(index + 1, random);
		[items[index], items[swapIndex]] = [items[swapIndex], items[index]];
	}
	return items;
}

function createWelcomeTextGroups(): ReadonlyArray<WelcomeTextGroup> {
	const groupsByText = new Map<string, Array<number>>();
	for (const [index, entry] of WELCOME_ROTATION.entries()) {
		const textKey = normalizeWelcomeText(entry.text);
		const group = groupsByText.get(textKey);
		if (group) {
			group.push(index);
			continue;
		}
		groupsByText.set(textKey, [index]);
	}
	return [...groupsByText.entries()].map(([textKey, indexes]) => ({textKey, indexes}));
}

const WELCOME_TEXT_GROUPS = createWelcomeTextGroups();
const WELCOME_TEXT_GROUPS_BY_INDEX = new Map<number, WelcomeTextGroup>(
	WELCOME_TEXT_GROUPS.flatMap((group) => group.indexes.map((index) => [index, group])),
);

function findWelcomeLocaleIndex(localeCode: string): number {
	const localeIndex = WELCOME_ROTATION.findIndex((entry) => entry.code === localeCode);
	if (localeIndex >= 0) return localeIndex;
	const defaultIndex = WELCOME_ROTATION.findIndex((entry) => entry.code === DEFAULT_WELCOME_LOCALE);
	return defaultIndex >= 0 ? defaultIndex : 0;
}

function pickWelcomeGroupIndex(group: WelcomeTextGroup, random: RandomSource): number {
	return group.indexes[randomWelcomeIndex(group.indexes.length, random)] ?? 0;
}

function createWelcomeOrder(
	random: RandomSource,
	options: {firstIndex?: number; previousIndex?: number} = {},
): ReadonlyArray<number> {
	const firstGroup =
		options.firstIndex === undefined ? undefined : WELCOME_TEXT_GROUPS_BY_INDEX.get(options.firstIndex);
	const previousGroup =
		options.previousIndex === undefined ? undefined : WELCOME_TEXT_GROUPS_BY_INDEX.get(options.previousIndex);
	const groups = shuffleWelcomeItems(
		WELCOME_TEXT_GROUPS.filter((group) => group.textKey !== firstGroup?.textKey),
		random,
	);

	if (previousGroup && groups.length > 1 && groups[0]?.textKey === previousGroup.textKey) {
		const replacementIndex = groups.findIndex((group) => group.textKey !== previousGroup.textKey);
		if (replacementIndex > 0) {
			[groups[0], groups[replacementIndex]] = [groups[replacementIndex], groups[0]];
		}
	}

	const order = groups.map((group) => pickWelcomeGroupIndex(group, random));
	if (options.firstIndex === undefined) return order;
	return [options.firstIndex, ...order];
}

export function welcomeTextForLocale(localeCode: string): string {
	return WELCOME_ROTATION[findWelcomeLocaleIndex(localeCode)]?.text ?? WELCOME_ROTATION[0].text;
}

export function createWelcomeRotationState(
	localeCode: string,
	random: RandomSource = Math.random,
): WelcomeRotationState {
	const firstIndex = findWelcomeLocaleIndex(localeCode);
	return {
		order: createWelcomeOrder(random, {firstIndex}),
		position: 0,
	};
}

export function createRandomWelcomeRotationState(
	previousIndex: number | undefined,
	random: RandomSource = Math.random,
): WelcomeRotationState {
	return {
		order: createWelcomeOrder(random, {previousIndex}),
		position: 0,
	};
}

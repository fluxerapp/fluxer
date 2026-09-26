// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserAgentResult} from '@app/api/risk/RiskTypes';

const BOT_PATTERNS: ReadonlyArray<RegExp> = [
	/\bbot\b/i,
	/\bcrawler\b/i,
	/\bspider\b/i,
	/\bscraper\b/i,
	/\bcurl\b/i,
	/\bwget\b/i,
	/\bhttpx\b/i,
	/\bpython[-\s]?requests\b/i,
	/\bnode[-\s]?fetch\b/i,
	/\bgot\b\/\d/i,
	/\baxios\b/i,
	/\bgo[-\s]?http[-\s]?client\b/i,
	/\bjava\b.*\bhttp\b/i,
	/\bphp\b/i,
	/\bperl\b/i,
	/\bruby\b/i,
	/\baiohttp\b/i,
	/\bscrapy\b/i,
	/\bpostman\b/i,
	/\binsomnia\b/i,
	/\bthunder\s*client\b/i,
];
const HEADLESS_PATTERNS: ReadonlyArray<RegExp> = [
	/\bHeadlessChrome\b/i,
	/\bHeadless\b/i,
	/\bPhantomJS\b/i,
	/\bSlimerJS\b/i,
	/\bNightmare\b/i,
];
const AUTOMATION_PATTERNS: ReadonlyArray<RegExp> = [
	/\bSelenium\b/i,
	/\bWebDriver\b/i,
	/\bPuppeteer\b/i,
	/\bPlaywright\b/i,
	/\bCypress\b/i,
	/\bTestCafe\b/i,
	/\bAppium\b/i,
	/\bautomation\b/i,
];
const OUTDATED_CHROME_MAJOR = 110;
const OUTDATED_FIREFOX_MAJOR = 110;
const CHROME_VERSION_RE = /Chrome\/(\d+)/;
const FIREFOX_VERSION_RE = /Firefox\/(\d+)/;
const SAFARI_VERSION_RE = /Version\/(\d+)\.\d.*Safari/;

function matchesAny(ua: string, patterns: ReadonlyArray<RegExp>): boolean {
	return patterns.some((p) => p.test(ua));
}

function detectBrowser(ua: string): {
	browser: string;
	version: string | null;
} {
	if (/Edg\/(\d[\d.]+)/.test(ua)) return {browser: 'Edge', version: RegExp.$1};
	if (/OPR\/(\d[\d.]+)/.test(ua)) return {browser: 'Opera', version: RegExp.$1};
	if (/Vivaldi\/(\d[\d.]+)/.test(ua)) return {browser: 'Vivaldi', version: RegExp.$1};
	if (/Brave/.test(ua) && CHROME_VERSION_RE.test(ua)) return {browser: 'Brave', version: RegExp.$1};
	if (/SamsungBrowser\/(\d[\d.]+)/.test(ua)) return {browser: 'Samsung Internet', version: RegExp.$1};
	if (CHROME_VERSION_RE.test(ua)) return {browser: 'Chrome', version: RegExp.$1};
	if (FIREFOX_VERSION_RE.test(ua)) return {browser: 'Firefox', version: RegExp.$1};
	if (SAFARI_VERSION_RE.test(ua)) return {browser: 'Safari', version: RegExp.$1};
	if (/MSIE|Trident/.test(ua)) return {browser: 'Internet Explorer', version: null};
	return {browser: 'unknown', version: null};
}

function detectOs(ua: string): string {
	if (/Windows NT 10/.test(ua)) return 'Windows 10/11';
	if (/Windows NT/.test(ua)) return 'Windows (older)';
	if (/Mac OS X/.test(ua)) return 'macOS';
	if (/CrOS/.test(ua)) return 'ChromeOS';
	if (/Android/.test(ua)) return 'Android';
	if (/iPhone|iPad|iPod/.test(ua)) return 'iOS';
	if (/Linux/.test(ua)) return 'Linux';
	return 'unknown';
}

function isOutdated(ua: string): boolean {
	const chromeMatch = ua.match(CHROME_VERSION_RE);
	if (chromeMatch) {
		const major = Number.parseInt(chromeMatch[1]!, 10);
		if (major > 0 && major < OUTDATED_CHROME_MAJOR) return true;
	}
	const firefoxMatch = ua.match(FIREFOX_VERSION_RE);
	if (firefoxMatch) {
		const major = Number.parseInt(firefoxMatch[1]!, 10);
		if (major > 0 && major < OUTDATED_FIREFOX_MAJOR) return true;
	}
	if (/MSIE|Trident/.test(ua)) return true;
	return false;
}

export function analyzeUserAgent(args: {userAgent: string}): UserAgentResult {
	const ua = args.userAgent;
	if (!ua || ua.length === 0) {
		return {
			raw: '',
			browser: 'missing',
			browserVersion: null,
			os: 'unknown',
			isBot: true,
			isHeadless: false,
			isAutomation: false,
			isOutdated: false,
			suspiciousPatterns: ['empty user-agent'],
			riskNote: 'User-Agent header is missing or empty — likely a script or bot',
		};
	}
	const isBot = matchesAny(ua, BOT_PATTERNS);
	const isHeadless = matchesAny(ua, HEADLESS_PATTERNS);
	const isAutomation = matchesAny(ua, AUTOMATION_PATTERNS);
	const outdated = isOutdated(ua);
	const {browser, version} = detectBrowser(ua);
	const os = detectOs(ua);
	const suspiciousPatterns: Array<string> = [];
	if (isBot) suspiciousPatterns.push('known bot/scraper user-agent');
	if (isHeadless) suspiciousPatterns.push('headless browser detected');
	if (isAutomation) suspiciousPatterns.push('automation framework detected');
	if (outdated) suspiciousPatterns.push('very outdated browser version');
	if (ua.length < 30 && !isBot) suspiciousPatterns.push('unusually short user-agent');
	let riskNote = 'no suspicious user-agent patterns';
	if (suspiciousPatterns.length > 0) {
		riskNote = suspiciousPatterns.join('; ');
	}
	return {
		raw: ua.length > 300 ? `${ua.slice(0, 300)}...` : ua,
		browser,
		browserVersion: version,
		os,
		isBot,
		isHeadless,
		isAutomation,
		isOutdated: outdated,
		suspiciousPatterns,
		riskNote,
	};
}

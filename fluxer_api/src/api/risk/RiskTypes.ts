// SPDX-License-Identifier: AGPL-3.0-or-later

export enum RiskLevel {
	Low = 'low',
	Medium = 'medium',
	High = 'high',
	VeryHigh = 'very_high',
}

export enum RiskConfidence {
	Low = 'low',
	Medium = 'medium',
	High = 'high',
}

export enum RiskDecisionMethod {
	Deterministic = 'deterministic',
	Noop = 'noop',
}

export interface RegistrationEvent {
	email: string | null;
	ip: string;
	locale: string | null;
	timezone: string | null;
	userAgent: string | null;
	username?: string | null;
	globalName?: string | null;
	usernameIsUserChosen?: boolean;
	isUnclaimed?: boolean;
}

export interface RiskSignals {
	emailSyntax?: EmailSyntaxResult;
	domainDisposable?: DisposableCheckResult;
	domainMx?: MxCheckResult;
	domainAge?: DomainAgeResult;
	geoIpCity?: GeoIpCityResult;
	geoIpAsn?: GeoIpAsnResult;
	ipInfoAnonymous?: IpInfoAnonymousResult;
	reverseDns?: ReverseDnsResult;
	suspiciousIp?: SuspiciousIpResult;
	registrationsByIp?: VelocityResult;
	registrationsBySubnet?: VelocityResult;
	registrationsByEmailDomain?: VelocityResult;
	registrationsByPlusAddressBase?: VelocityResult;
	historicalOutcomesByIp?: HistoricalOutcomeResult;
	historicalOutcomesBySubnet?: HistoricalOutcomeResult;
	historicalOutcomesByEmailDomain?: HistoricalOutcomeResult;
	historicalOutcomesByAsn?: HistoricalOutcomeResult;
	frequentAbuserByIp?: HistoricalOutcomeResult;
	frequentAbuserBySubnet?: HistoricalOutcomeResult;
	localeGeoMatch?: LocaleGeoMatchResult;
	userAgent?: UserAgentResult;
	registrationTiming?: RegistrationTimingResult;
}

export interface RiskAssessment {
	suspicious: boolean;
	level: RiskLevel;
	confidence: RiskConfidence;
	riskScore: number;
	reasoning: string;
	recommendedAction: RecommendedAction;
	method: RiskDecisionMethod;
	modelUsed: string;
	rounds: number;
	elapsedMs: number;
	signals: RiskSignals;
}

export enum RecommendedAction {
	Allow = 'allow',
	RequireVerifiedEmail = 'require_verified_email',
	RequireOutboundPhone = 'require_outbound_phone',
	RequireInboundPhone = 'require_inbound_phone',
	Block = 'block',
}

export interface EmailSyntaxResult {
	email: string;
	localPart: string;
	domain: string;
	localPartLength: number;
	entropy: number;
	keyboardMashDetected: boolean;
	looksLikeName: boolean;
	pronounceability: number;
	unusualRepeats: boolean;
	digitRatio: number;
	hasDots: boolean;
	hasPlusTag: boolean;
	gibberishScore: number;
	valid: boolean;
}

export interface DisposableCheckResult {
	domain: string;
	isDisposable: boolean;
}

export interface MxCheckResult {
	domain: string;
	hasMx: boolean;
	recordCount: number;
	records: ReadonlyArray<{
		priority: number;
		host: string;
	}>;
	error: string | null;
}

export interface GeoIpCityResult {
	ip: string;
	available: boolean;
	found: boolean;
	countryIso: string | null;
	country: string | null;
	region: string | null;
	city: string | null;
	latitude: number | null;
	longitude: number | null;
	accuracyRadiusKm: number | null;
	timeZone: string | null;
}

export interface GeoIpAsnResult {
	ip: string;
	available: boolean;
	found: boolean;
	asn: number | null;
	asnOrg: string | null;
}

export type IpConnectionType =
	| 'residential'
	| 'residential_proxy'
	| 'mobile'
	| 'data_center'
	| 'corporate'
	| 'education'
	| 'unknown';

export interface IpInfoAnonymousResult {
	ip: string;
	available: boolean;
	isAnonymous: boolean;
	providerName: string | null;
	isVpn: boolean;
	isProxy: boolean;
	isResidentialProxy: boolean;
	isTor: boolean;
	isRelay: boolean;
	isHosting: boolean;
	isMobile: boolean;
	asnType: string | null;
	asnOrg: string | null;
	connectionType: IpConnectionType;
	percentDaysSeen: number | null;
	riskNote: string;
}

export type ReverseDnsClassification = 'dynamic' | 'static' | 'cellular' | 'business' | 'unknown';

export interface ReverseDnsResult {
	ip: string;
	hostname: string | null;
	classification: ReverseDnsClassification;
}

export interface SuspiciousIpResult {
	ip: string;
	source: string;
	reason: string;
	sourceUserId: string | null;
	deletionReasonCode: number | null;
	createdAt: string;
	expiresAt: string | null;
	riskNote: string;
}

export interface VelocityResult {
	identifier: string;
	windowHours: number;
	totalRegistrations: number;
	truncated: boolean;
	uniqueEmails: number;
	uniqueLocales: ReadonlyArray<string>;
	uniqueIps: number;
	riskNote: string;
}

export interface HistoricalOutcomeResult {
	identifier: string;
	windowHours: number;
	sampledRegistrations: number;
	truncated: boolean;
	sampledUsers: number;
	resolvedUsers: number;
	adverseUsers: number;
	challengedUsers: number;
	enforcedUsers: number;
	spammerUsers: number;
	disabledUsers: number;
	disabledSuspiciousUsers: number;
	riskNote: string;
}

export interface LocaleGeoMatchResult {
	geoipCountryIso: string | null;
	registrationLocale: string | null;
	registrationTimezone: string | null;
	localeGeoMatch: boolean | null;
	timezoneGeoMatch: boolean | null;
	mismatchDetected: boolean;
	notes: ReadonlyArray<string>;
}

export interface UserAgentResult {
	raw: string;
	browser: string;
	browserVersion: string | null;
	os: string;
	isBot: boolean;
	isHeadless: boolean;
	isAutomation: boolean;
	isOutdated: boolean;
	suspiciousPatterns: ReadonlyArray<string>;
	riskNote: string;
}

export interface DomainAgeResult {
	domain: string;
	available: boolean;
	creationDate: string | null;
	ageDays: number | null;
	isNewlyRegistered: boolean;
	riskNote: string;
}

export interface RegistrationTimingResult {
	timezone: string | null;
	localHour: number | null;
	isSuspiciousHour: boolean;
	riskNote: string;
}

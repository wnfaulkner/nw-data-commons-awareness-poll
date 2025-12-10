# Protocol Version Control System - Complete Implementation Prompt

## Purpose

Implement a comprehensive decision-making logging and version control system for tracking substantive technical, architectural, or methodological changes throughout a project lifecycle. This system provides:

- **Audit trail**: Complete history of all significant decisions with rationale
- **Reproducibility**: Document exactly why and when changes were made
- **Transparency**: Clear evidence for peer review, stakeholders, or future maintainers
- **Version tracking**: Semantic versioning of your methodology/architecture
- **Accountability**: Structured documentation of options considered and trade-offs

Originally designed for academic research protocols (PRISMA-S compliance), this system is equally valuable for:
- Software architecture evolution
- API design decisions
- Algorithm selection and tuning
- Database schema changes
- Infrastructure migrations
- Technical debt management
- Any project requiring defensible decision documentation

---

## System Architecture

### Directory Structure

```
project_root/
├── protocol/                          # Protocol version control directory
│   ├── README.md                      # System documentation and usage guide
│   ├── protocol_v1.0.md              # Locked baseline (NEVER modified)
│   ├── protocol.md                    # Living document (current version)
│   ├── decision_log.json             # Structured machine-readable log
│   ├── CHANGELOG.md                   # Human-readable summary
│   └── versions/                      # Archived versions
│       ├── protocol_v1.1.md
│       ├── protocol_v1.2.md
│       └── ...
└── utils/
    └── protocol_logger.py             # Automation tool
```

### Core Components

1. **`protocol.md`** - Living protocol document
   - Current active methodology/architecture
   - Updated when substantive changes occur
   - Semantic version number in header
   - Working reference for all project activities

2. **`protocol_v1.0.md`** - Locked baseline
   - Original protocol/architecture document
   - NEVER modified after creation
   - Reference point for tracking evolution
   - Comparison baseline for deviation analysis

3. **`decision_log.json`** - Structured decision history
   - Machine-readable JSON format
   - Every substantive decision with timestamp
   - Includes: trigger, evidence, options, decision, rationale, impact
   - Queryable for analysis and reporting

4. **`CHANGELOG.md`** - Human-readable summary
   - Follows "Keep a Changelog" format
   - Chronological list of changes
   - Links to decision log for details
   - Quick overview of evolution

5. **`versions/`** - Archived snapshots
   - Previous protocol versions
   - Saved before each update
   - Named by version number
   - Supports point-in-time reconstruction

6. **`utils/protocol_logger.py`** - Automation tool
   - Python utility for logging decisions
   - Handles versioning automatically
   - Updates all files consistently
   - Enforces structured format

---

## Implementation

### Step 1: Create Directory Structure

Create the following directories:

```bash
mkdir -p protocol/versions
mkdir -p utils
```

### Step 2: Create `utils/protocol_logger.py`

```python
"""
Protocol Version Control and Decision Logging

Automated tracking of substantive methodological/technical changes for:
- Audit trail and reproducibility
- Transparent decision documentation
- Protocol/architecture evolution tracking
- Compliance (PRISMA-S, SOC2, etc.)

Usage:
    from utils.protocol_logger import log_decision

    log_decision(
        stage="Stage 3 - Data Collection",
        trigger="API rate limits prevent full daily sync",
        evidence={"daily_records": 100000, "api_limit": 10000},
        options=[
            "Option A: Incremental sync only",
            "Option B: Batch processing with queueing"
        ],
        decision="Option B",
        rationale="Incremental sync would create data gaps",
        protocol_sections=["3.2 - Data Ingestion Strategy"],
        implementation={
            "scripts_created": ["scripts/batch_processor.py"],
            "dependencies_added": ["redis", "celery"]
        }
    )
"""

import json
import shutil
from pathlib import Path
from datetime import datetime, timezone
from typing import List, Dict, Any, Optional


class ProtocolLogger:
    """Manages protocol versioning and decision logging."""

    def __init__(self, protocol_dir: Path = None):
        """
        Initialize protocol logger.

        Args:
            protocol_dir: Path to protocol directory (default: project_root/protocol)
        """
        if protocol_dir is None:
            # Assume this file is in utils/, so project root is parent
            project_root = Path(__file__).parent.parent
            protocol_dir = project_root / "protocol"

        self.protocol_dir = Path(protocol_dir)
        self.decision_log_path = self.protocol_dir / "decision_log.json"
        self.changelog_path = self.protocol_dir / "CHANGELOG.md"
        self.protocol_path = self.protocol_dir / "protocol.md"
        self.versions_dir = self.protocol_dir / "versions"

        # Ensure directories exist
        self.protocol_dir.mkdir(exist_ok=True)
        self.versions_dir.mkdir(exist_ok=True)

    def _load_decision_log(self) -> Dict[str, Any]:
        """Load existing decision log or create new one."""
        if self.decision_log_path.exists():
            with open(self.decision_log_path, 'r') as f:
                return json.load(f)
        else:
            return {
                "metadata": {
                    "project": "Project Name",
                    "protocol_baseline_version": "1.0",
                    "protocol_current_version": "1.0",
                    "created": datetime.now(timezone.utc).isoformat(),
                    "last_updated": datetime.now(timezone.utc).isoformat(),
                    "total_decisions": 0
                },
                "decisions": []
            }

    def _save_decision_log(self, log: Dict[str, Any]) -> None:
        """Save decision log to JSON file."""
        with open(self.decision_log_path, 'w') as f:
            json.dump(log, f, indent=2, ensure_ascii=False)

    def _generate_decision_id(self, log: Dict[str, Any]) -> str:
        """Generate next decision ID (e.g., DEC-001, DEC-002)."""
        count = log["metadata"]["total_decisions"] + 1
        return f"DEC-{count:03d}"

    def _increment_version(self, current_version: str, change_type: str = "minor") -> str:
        """
        Increment semantic version.

        Args:
            current_version: Current version string (e.g., "1.2")
            change_type: "major" or "minor"

        Returns:
            New version string
        """
        parts = current_version.split('.')
        major = int(parts[0])
        minor = int(parts[1]) if len(parts) > 1 else 0

        if change_type == "major":
            return f"{major + 1}.0"
        else:  # minor
            return f"{major}.{minor + 1}"

    def _archive_protocol_version(self, old_version: str) -> None:
        """Archive current protocol before making changes."""
        if self.protocol_path.exists():
            archive_path = self.versions_dir / f"protocol_v{old_version}.md"
            shutil.copy2(self.protocol_path, archive_path)
            print(f"✓ Archived protocol v{old_version} to {archive_path}")

    def _update_protocol_version_header(self, new_version: str) -> None:
        """Update version number in protocol.md header."""
        if not self.protocol_path.exists():
            return

        with open(self.protocol_path, 'r') as f:
            content = f.read()

        # Update version line
        lines = content.split('\n')
        for i, line in enumerate(lines):
            if line.startswith('**Version:**'):
                lines[i] = f'**Version:** {new_version}'
            elif line.startswith('**Last Updated:**'):
                lines[i] = f'**Last Updated:** {datetime.now(timezone.utc).strftime("%Y-%m-%d")}'

        with open(self.protocol_path, 'w') as f:
            f.write('\n'.join(lines))

        print(f"✓ Updated protocol.md version header to v{new_version}")

    def _update_changelog(self, decision: Dict[str, Any], new_version: str) -> None:
        """Add entry to CHANGELOG.md."""
        if not self.changelog_path.exists():
            return

        # Read existing changelog
        with open(self.changelog_path, 'r') as f:
            content = f.read()

        # Create new entry
        entry = f"\n### Changed\n- {decision['trigger']['description']} ([{decision['id']}])\n  - **Decision:** {decision['decision']}\n  - **Rationale:** {decision['rationale']}\n  - **Impact:** Protocol sections {', '.join(decision['protocol_impact']['sections_modified'])}\n"

        # Insert after [Unreleased] header
        unreleased_pos = content.find('## [Unreleased]')
        if unreleased_pos != -1:
            # Find end of Unreleased section
            next_version_pos = content.find('\n## [', unreleased_pos + 1)
            if next_version_pos != -1:
                # Insert before next version
                content = content[:next_version_pos] + entry + content[next_version_pos:]
            else:
                # Append to end of Unreleased
                content = content + entry

        with open(self.changelog_path, 'w') as f:
            f.write(content)

        print(f"✓ Updated CHANGELOG.md with decision {decision['id']}")

    def log_decision(
        self,
        stage: str,
        trigger: str,
        evidence: Dict[str, Any],
        options: List[str],
        decision: str,
        rationale: str,
        protocol_sections: List[str],
        implementation: Optional[Dict[str, Any]] = None,
        change_type: str = "minor"
    ) -> str:
        """
        Log a substantive protocol decision.

        Args:
            stage: Which stage/phase (e.g., "Stage 3 - Data Collection")
            trigger: What prompted this decision
            evidence: Data supporting the need for decision
            options: List of options considered
            decision: Final decision made
            rationale: Why this decision was made
            protocol_sections: Which sections are affected
            implementation: Implementation details (scripts, dependencies, etc.)
            change_type: "major" or "minor" (default: "minor")

        Returns:
            Decision ID (e.g., "DEC-001")
        """
        # Load existing log
        log = self._load_decision_log()

        # Generate decision ID
        decision_id = self._generate_decision_id(log)

        # Determine version change
        old_version = log["metadata"]["protocol_current_version"]
        new_version = self._increment_version(old_version, change_type)

        # Create decision entry
        decision_entry = {
            "id": decision_id,
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "stage": stage,
            "trigger": {
                "description": trigger,
                "evidence": evidence
            },
            "options_considered": [{"option": opt} for opt in options],
            "decision": decision,
            "rationale": rationale,
            "protocol_impact": {
                "sections_modified": protocol_sections,
                "version_change": f"{old_version} → {new_version}",
                "methodology_change": decision
            },
            "implementation": implementation or {}
        }

        # Add to log
        log["decisions"].append(decision_entry)
        log["metadata"]["total_decisions"] += 1
        log["metadata"]["protocol_current_version"] = new_version
        log["metadata"]["last_updated"] = datetime.now(timezone.utc).isoformat()

        # Archive old protocol version
        if protocol_sections:  # Only archive if protocol sections modified
            self._archive_protocol_version(old_version)
            self._update_protocol_version_header(new_version)

        # Save updated log
        self._save_decision_log(log)

        # Update changelog
        self._update_changelog(decision_entry, new_version)

        # Print summary
        print("\n" + "=" * 80)
        print(f"SUBSTANTIVE DECISION LOGGED: {decision_id}")
        print("=" * 80)
        print(f"Stage: {stage}")
        print(f"Trigger: {trigger}")
        print(f"Decision: {decision}")
        print(f"Protocol version: {old_version} → {new_version}")
        print(f"Sections affected: {', '.join(protocol_sections)}")
        print("=" * 80 + "\n")

        return decision_id


# Convenience function for easy imports
def log_decision(**kwargs) -> str:
    """
    Convenience function for logging decisions.

    See ProtocolLogger.log_decision() for full documentation.
    """
    logger = ProtocolLogger()
    return logger.log_decision(**kwargs)


# Example usage
if __name__ == "__main__":
    # Example: Logging a decision about caching strategy
    decision_id = log_decision(
        stage="Architecture - Caching Layer",
        trigger="Response times exceed 500ms SLA for 40% of requests",
        evidence={
            "p95_response_time_ms": 850,
            "sla_target_ms": 500,
            "requests_exceeding_sla_pct": 40,
            "cache_hit_rate": 0.15
        },
        options=[
            "Option A: Redis distributed cache",
            "Option B: In-memory cache with eventual consistency",
            "Option C: CDN caching for static content only",
            "Option D: Hybrid - Redis + CDN"
        ],
        decision="Option D - Hybrid Redis + CDN",
        rationale="Redis handles dynamic content with low latency. CDN offloads static assets. Combined approach addresses both use cases without over-engineering.",
        protocol_sections=["Architecture - Caching Strategy"],
        implementation={
            "dependencies_added": ["redis", "hiredis", "cloudflare-sdk"],
            "config_files": ["config/redis.yaml", "config/cdn.yaml"],
            "scripts_created": ["scripts/cache_warmup.py"],
            "estimated_improvement": "p95 response time < 200ms"
        },
        change_type="minor"
    )

    print(f"Logged decision: {decision_id}")
    print("Check protocol/decision_log.json for details")
```

### Step 3: Create `protocol/README.md`

```markdown
# Protocol Version Control System

This directory contains the project protocol/architecture and automated tracking of all substantive technical changes.

## Purpose

- **Audit trail**: Complete history of decisions
- **Reproducibility**: Document exactly why and when changes were made
- **Transparency**: Evidence for stakeholders and future maintainers
- **Compliance**: Supports various documentation requirements

## Files

### Core Documents

**`protocol.md`** - Living protocol (CURRENT VERSION)
- Reflects the active methodology/architecture
- Updated when substantive changes are made
- Version number increments with each change
- Working reference for all project activities

**`protocol_v1.0.md`** - Locked baseline (NEVER MODIFIED)
- Original protocol/architecture document
- Reference point for tracking evolution
- Comparison baseline for deviation analysis

**`decision_log.json`** - Structured decision history (MACHINE-READABLE)
- Every substantive decision logged with:
  - Timestamp and decision ID
  - Stage/phase of project
  - Trigger (what prompted the decision)
  - Evidence (data supporting the decision)
  - Options considered
  - Final decision and rationale
  - Sections affected
  - Implementation details
- Queryable for analysis and reporting
- Automatically updated by `utils/protocol_logger.py`

**`CHANGELOG.md`** - Human-readable change summary
- Follows "Keep a Changelog" format
- Chronological list of changes
- Links to decision log entries for details
- Quick overview of evolution

### Archive

**`versions/`** - Archived protocol versions
- Previous versions saved before each update
- Named by version number (e.g., `protocol_v1.1.md`)
- Allows point-in-time reconstruction
- Supports tracing decisions to specific states

## Version Numbering

**Semantic versioning:** `MAJOR.MINOR`

- **Major (X.0)**: Fundamental changes
  - Examples: Complete architecture overhaul, technology stack change
  - Rare, requires strong justification

- **Minor (1.X)**: Substantive but incremental changes
  - Examples: Adding features, modifying algorithms, changing infrastructure
  - Most decisions fall into this category

## What Gets Logged?

### Substantive Changes (LOGGED)

Changes that alter methodology, architecture, or affect reproducibility:

1. **Technical scope/methods:**
   - Changes to architecture or design patterns
   - Addition or removal of features
   - Changes to algorithms or data structures
   - Modifications to APIs or interfaces

2. **Reproducibility/validity:**
   - Changes to testing strategies
   - Modifications to deployment procedures
   - Changes to security policies
   - Infrastructure modifications

3. **Deviations from plan:**
   - Solutions to unforeseen problems
   - Adaptations based on empirical findings
   - Technical refinements from real-world use
   - Changes from discovered constraints

### Non-Substantive Changes (NOT LOGGED)

- Typo corrections
- Formatting improvements
- Clarifications without meaning changes
- Addition of examples/elaborations
- Reorganization without content changes

## How to Use

### Logging a Decision

```python
from utils.protocol_logger import log_decision

log_decision(
    stage="Feature Development - Authentication",
    trigger="Password-only auth insufficient for compliance requirements",
    evidence={"compliance": "SOC2", "requirement": "MFA mandatory"},
    options=[
        "Option A: SMS-based 2FA",
        "Option B: TOTP authenticator apps",
        "Option C: WebAuthn/FIDO2"
    ],
    decision="Option C - WebAuthn/FIDO2",
    rationale="Most secure, phishing-resistant, better UX than SMS",
    protocol_sections=["Authentication Architecture"],
    implementation={
        "dependencies_added": ["webauthn"],
        "affected_components": ["login", "registration", "settings"]
    }
)
```

### Reviewing Decisions

- Read `CHANGELOG.md` for quick summary
- Check `decision_log.json` for full details
- Compare `protocol.md` to `protocol_v1.0.md` for cumulative changes
- Review archived versions in `versions/` for historical states

## Example Workflow

1. **Identify issue:** "API response times exceed SLA"
2. **Trigger logging:** Call `log_decision()` with all details
3. **System automatically:**
   - Generates decision ID (e.g., DEC-001)
   - Archives current protocol version
   - Updates protocol.md to v1.1
   - Adds entry to decision_log.json
   - Updates CHANGELOG.md
   - Prints confirmation
4. **Result:** Complete audit trail maintained

## Testing

Test the logger:

```bash
python utils/protocol_logger.py
```

This runs an example decision and verifies all components work correctly.
```

### Step 4: Create `protocol/CHANGELOG.md`

```markdown
# Protocol Changelog

All substantive changes to the protocol are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project uses [Semantic Versioning](https://semver.org/spec/v2.0.0.html) for protocol versions.

---

## [Unreleased]

### Changed
- Protocol version control system implemented (YYYY-MM-DD)

---

## [1.0] - YYYY-MM-DD

### Added
- Initial protocol/architecture baseline
- [List key components of v1.0 here]

---

## Version Number Scheme

**Major version (X.0):** Fundamental changes (e.g., complete architecture overhaul, technology stack change)

**Minor version (1.X):** Substantive but incremental changes (e.g., adding features, modifying algorithms, infrastructure changes)

---

## Change Categories

Changes are categorized using these tags:
- **Added:** New sections, methods, or procedures
- **Changed:** Modifications to existing methods
- **Deprecated:** Methods marked for future removal
- **Removed:** Deleted sections or methods
- **Fixed:** Corrections to errors in the protocol
- **Security:** Changes related to security

---

## Decision Log Reference

For detailed rationale and evidence supporting each change, see `decision_log.json`.

Each decision includes:
- Trigger (problem or finding)
- Options considered
- Decision made
- Rationale
- Evidence
- Implementation details

---

**Note:** This changelog is automatically updated by `utils/protocol_logger.py` when decisions are logged.
```

### Step 5: Create Initial `protocol/decision_log.json`

```json
{
  "metadata": {
    "project": "Your Project Name",
    "protocol_baseline_version": "1.0",
    "protocol_current_version": "1.0",
    "created": "YYYY-MM-DDTHH:MM:SSZ",
    "last_updated": "YYYY-MM-DDTHH:MM:SSZ",
    "total_decisions": 0
  },
  "decisions": []
}
```

### Step 6: Create `protocol/protocol.md`

```markdown
# Project Protocol v1.0 - LIVING DOCUMENT

**Version:** 1.0
**Status:** CURRENT - This document reflects the active protocol
**Last Updated:** YYYY-MM-DD
**Baseline:** `protocol_v1.0.md`

---

## Document Purpose

This is the **living protocol** that reflects the current methodology/architecture. It is updated as substantive decisions are made during the project. All changes are:
- Logged in `decision_log.json` with full rationale
- Summarized in `CHANGELOG.md` in human-readable format
- Archived versions saved in `protocol/versions/` before each update

**Baseline reference:** See `protocol_v1.0.md` for the original, unmodified protocol.

---

## Definition of Substantive Changes

**Substantive changes** are technical/methodological modifications that:

1. **Alter scope or methods:**
   - Changes to architecture or design patterns
   - Addition or removal of features/components
   - Changes to algorithms or data structures
   - Modifications to APIs or interfaces

2. **Affect reproducibility or validity:**
   - Changes to testing strategies
   - Modifications to deployment procedures
   - Changes to security/compliance policies
   - Infrastructure or tooling changes

3. **Deviate from original protocol:**
   - Solutions to unforeseen implementation problems
   - Adaptations based on empirical findings
   - Technical refinements informed by real-world use
   - Changes necessitated by discovered constraints

**Non-substantive changes** (not logged):
- Typo corrections
- Formatting improvements
- Clarifications that don't change meaning
- Addition of examples or elaborations
- Reorganization without content changes

---

## [Your Project Content Here]

### Architecture Overview
[Describe your system architecture]

### Components
[List and describe key components]

### Data Flow
[Describe how data moves through the system]

### API Design
[Document API contracts]

### Deployment Strategy
[Describe how the system is deployed]

### Testing Strategy
[Document testing approach]

### Security Considerations
[Document security measures]

---

## Version History

**v1.0 (YYYY-MM-DD):** Initial protocol baseline

---

**END OF BASELINE PROTOCOL v1.0**
```

### Step 7: Create `protocol/protocol_v1.0.md`

Copy `protocol.md` to `protocol_v1.0.md`:

```bash
cp protocol/protocol.md protocol/protocol_v1.0.md
```

This locked baseline is NEVER modified after creation.

---

## Usage Examples

### Example 1: Logging a Database Schema Change

```python
from utils.protocol_logger import log_decision

log_decision(
    stage="Database Schema - User Table",
    trigger="Need to support multi-tenancy, current schema is single-tenant",
    evidence={
        "current_users": 15000,
        "tenant_count": 50,
        "data_isolation_required": True,
        "performance_target": "< 100ms query time"
    },
    options=[
        "Option A: Separate database per tenant (full isolation)",
        "Option B: Shared database, tenant_id column (best performance)",
        "Option C: Shared database, schema per tenant (hybrid)",
        "Option D: Hybrid - shared DB for small tenants, dedicated for large"
    ],
    decision="Option C - Schema per tenant",
    rationale="Balances isolation (compliance requirement) with operational complexity. Better than Option A (too many databases) and Option B (insufficient isolation). Option D adds unnecessary complexity for current scale.",
    protocol_sections=["3.2 Database Architecture"],
    implementation={
        "migration_scripts": ["migrations/2024_01_multi_tenant.sql"],
        "affected_tables": ["users", "sessions", "audit_log"],
        "rollback_plan": "Schema can be merged back to single if needed",
        "testing": "Load test with 100 schemas"
    },
    change_type="minor"
)
```

### Example 2: Logging an API Breaking Change

```python
from utils.protocol_logger import log_decision

log_decision(
    stage="API Design - Authentication Endpoint",
    trigger="Current token format doesn't support granular permissions",
    evidence={
        "token_format": "opaque UUID",
        "permission_model": "role-based only",
        "requirement": "resource-level permissions needed",
        "clients_affected": 12
    },
    options=[
        "Option A: Add permissions to existing tokens (backwards compatible)",
        "Option B: Introduce JWT with claims (breaking change)",
        "Option C: Dual token system (complexity overhead)"
    ],
    decision="Option B - JWT with claims",
    rationale="Breaking change justified by compliance requirements. Opaque tokens cannot encode permissions without DB lookup (latency). JWT enables stateless authorization. 3-month migration period announced to all clients.",
    protocol_sections=["2.1 Authentication", "2.3 Authorization"],
    implementation={
        "new_endpoints": ["/v2/auth/token"],
        "deprecated_endpoints": ["/v1/auth/token"],
        "migration_guide": "docs/api_v2_migration.md",
        "deprecation_timeline": "2024-06-01",
        "sunset_date": "2024-09-01"
    },
    change_type="major"  # Breaking change
)
```

### Example 3: Logging Algorithm Selection

```python
from utils.protocol_logger import log_decision

log_decision(
    stage="Feature - Recommendation Engine",
    trigger="Need personalized recommendations for 1M+ users",
    evidence={
        "user_count": 1200000,
        "items_catalog": 500000,
        "update_frequency": "hourly",
        "latency_requirement": "< 50ms p95"
    },
    options=[
        "Option A: Collaborative filtering (memory-based)",
        "Option B: Matrix factorization (ALS)",
        "Option C: Neural collaborative filtering",
        "Option D: Hybrid content + collaborative"
    ],
    decision="Option B - Matrix factorization (ALS)",
    rationale="Best trade-off for scale. Option A doesn't scale to 1M users. Option C requires GPU infrastructure (cost). Option D over-engineered for current needs. ALS proven at scale (Spotify, Netflix), meets latency requirements with pre-computation.",
    protocol_sections=["4.3 Recommendation Algorithm"],
    implementation={
        "library": "implicit",
        "model_params": {"factors": 100, "iterations": 15, "regularization": 0.01},
        "retraining_schedule": "daily 3am UTC",
        "serving_infrastructure": "Redis cache",
        "fallback_strategy": "popularity-based"
    },
    change_type="minor"
)
```

---

## Integration with AI Assistants (Claude Code, GitHub Copilot, etc.)

### Triggering Automatic Logging

When working with AI coding assistants, establish this pattern:

**User message format:**
```
SUBSTANTIVE: [description of decision needed]
```

**Example:**
```
SUBSTANTIVE: We need to decide how to handle rate limiting for our API.
Current approach (IP-based) doesn't work with our proxy setup.
```

**Assistant should:**
1. Recognize "SUBSTANTIVE" flag
2. Analyze the situation
3. Present options with trade-offs
4. After user decides, call `log_decision()` automatically
5. Confirm logging with decision ID

### Assistant Instructions

Include this in your AI assistant's system prompt or project instructions:

```
When you encounter a "SUBSTANTIVE" flag or identify a significant technical decision:

1. Present options clearly with trade-offs
2. After the user makes a decision, automatically log it using:
   from utils.protocol_logger import log_decision

3. Include all required fields:
   - stage: Current project phase/component
   - trigger: What prompted this decision
   - evidence: Concrete data (metrics, counts, constraints)
   - options: All alternatives considered
   - decision: What was chosen
   - rationale: Why this choice
   - protocol_sections: Which parts of protocol are affected
   - implementation: Technical details (scripts, dependencies, config)

4. Confirm with decision ID and version increment
```

---

## Best Practices

### When to Log

**DO log:**
- Architecture changes (microservices → monolith, SQL → NoSQL)
- API design changes (endpoints, contracts, breaking changes)
- Algorithm selection (which ML model, which sorting algorithm)
- Infrastructure changes (cloud provider, deployment strategy)
- Security decisions (auth method, encryption approach)
- Performance optimizations with trade-offs
- Technical debt decisions (when to refactor, what to postpone)

**DON'T log:**
- Routine bug fixes (unless they reveal architectural issues)
- Code style changes
- Dependency version bumps (unless major migration)
- Documentation improvements
- Test additions (unless methodology changes)

### Evidence Quality

Good evidence is:
- **Quantitative**: "p95 latency = 850ms" not "slow"
- **Specific**: "12 affected clients" not "some users"
- **Measurable**: "40% cache hit rate" not "low performance"
- **Contextual**: Include targets/SLAs for comparison

### Option Presentation

Present options with:
- **Pros and cons**: What are the trade-offs?
- **Costs**: Implementation effort, runtime cost, maintenance
- **Risks**: What could go wrong?
- **Precedents**: Has this been done before? By whom?

### Rationale Strength

Strong rationale includes:
- **Why this option**: Specific benefits
- **Why not others**: Explicit rejection reasons
- **Trade-offs accepted**: What are we sacrificing?
- **Assumptions**: What are we betting on?
- **Exit strategy**: How do we reverse this if wrong?

---

## Querying Decision History

### Find decisions by stage

```python
import json
from pathlib import Path

with open('protocol/decision_log.json') as f:
    log = json.load(f)

# Find all database decisions
db_decisions = [
    d for d in log['decisions']
    if 'database' in d['stage'].lower()
]

for dec in db_decisions:
    print(f"{dec['id']}: {dec['decision']}")
```

### Find decisions by timeframe

```python
from datetime import datetime

# Find decisions from last 30 days
now = datetime.now()
recent = [
    d for d in log['decisions']
    if (now - datetime.fromisoformat(d['timestamp'])).days <= 30
]
```

### Generate decision report

```python
# Generate markdown report
report = f"# Decision Report\n\n"
report += f"Total decisions: {log['metadata']['total_decisions']}\n"
report += f"Current version: {log['metadata']['protocol_current_version']}\n\n"

for dec in log['decisions']:
    report += f"## {dec['id']}: {dec['stage']}\n"
    report += f"**Decision:** {dec['decision']}\n"
    report += f"**Rationale:** {dec['rationale']}\n"
    report += f"**Date:** {dec['timestamp']}\n\n"

with open('docs/decision_report.md', 'w') as f:
    f.write(report)
```

---

## Compliance and Reporting

### For Peer Review (Academic)

The decision log supports:
- **PRISMA-S**: Protocol deviations documented with rationale
- **CONSORT**: Methodology changes during trials
- **Reproducibility**: Complete methodology evolution trail

### For Audits (Industry)

The decision log supports:
- **SOC 2**: Evidence of change management process
- **ISO 27001**: Documentation of security decisions
- **GDPR**: Privacy-by-design decision documentation
- **FDA/GxP**: Audit trail for validation changes

### For Stakeholders

Generate executive summaries:
```python
# Count decisions by type
major_changes = sum(1 for d in log['decisions'] if 'major' in d['protocol_impact']['version_change'])
minor_changes = log['metadata']['total_decisions'] - major_changes

print(f"Protocol Evolution: v1.0 → v{log['metadata']['protocol_current_version']}")
print(f"Major changes: {major_changes}")
print(f"Minor changes: {minor_changes}")
print(f"Last updated: {log['metadata']['last_updated']}")
```

---

## Customization

### Adapt for Your Domain

**For software architecture:**
- Rename `protocol.md` to `architecture.md`
- Focus on: components, APIs, data flow, deployment

**For data science:**
- Focus on: model selection, feature engineering, validation strategy
- Add fields for: datasets, hyperparameters, evaluation metrics

**For infrastructure:**
- Focus on: cloud resources, networking, security, monitoring
- Add fields for: cost impact, reliability impact, compliance

### Extend the Schema

Add custom fields to decision entries:

```python
# In protocol_logger.py, extend decision_entry dict:
decision_entry.update({
    "cost_impact": {
        "implementation_hours": 40,
        "ongoing_monthly_cost": 500,
        "cost_savings": 2000
    },
    "risk_assessment": {
        "probability": "low",
        "impact": "high",
        "mitigation": "Feature flags for gradual rollout"
    }
})
```

---

## Maintenance

### Regular Reviews

Schedule quarterly reviews:
1. Read through all decisions from last quarter
2. Identify patterns (are we making too many major changes?)
3. Check if rationales still hold (have assumptions changed?)
4. Update protocol.md if context has changed

### Archive Old Decisions

For long-running projects, consider archiving old decisions:

```python
# Archive decisions older than 2 years
from datetime import datetime, timedelta

cutoff = datetime.now() - timedelta(days=730)
active_decisions = [
    d for d in log['decisions']
    if datetime.fromisoformat(d['timestamp']) > cutoff
]

archived_decisions = [
    d for d in log['decisions']
    if datetime.fromisoformat(d['timestamp']) <= cutoff
]

# Save archive
with open('protocol/decision_log_archive_2022.json', 'w') as f:
    json.dump({"decisions": archived_decisions}, f, indent=2)
```

---

## Troubleshooting

### "Protocol file not found"

Ensure `protocol/protocol.md` exists. Create from template if missing.

### "Version mismatch"

If `decision_log.json` version doesn't match `protocol.md` header, manually sync:
1. Check latest decision in `decision_log.json`
2. Update `protocol.md` header to match
3. Or vice versa if decision_log is wrong

### "Archive failed"

Check write permissions on `protocol/versions/` directory.

---

## Summary

This system provides:
✓ Complete audit trail of decisions
✓ Semantic versioning of your protocol/architecture
✓ Structured, queryable decision history
✓ Human-readable change summaries
✓ Point-in-time protocol reconstruction
✓ Compliance documentation support
✓ Integration with AI coding assistants

**Start using it whenever you face a significant technical decision that affects your project's direction, architecture, or methodology.**

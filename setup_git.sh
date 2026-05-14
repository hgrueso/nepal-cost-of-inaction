#!/usr/bin/env bash
# ──────────────────────────────────────────────────────────────────────────────
# setup_git.sh — One-shot GitHub repository creation for Nepal CoI Analysis
# Requires: gh CLI authenticated (gh auth status to verify)
# Usage: bash setup_git.sh
# ──────────────────────────────────────────────────────────────────────────────

set -e

REPO_NAME="nepal-cost-of-inaction"
REPO_DESC="Cost-of-Inaction Analysis for Adolescent Girls' Education in Nepal — NLSS IV 2022/23"
VISIBILITY="public"  # change to "private" if needed

echo "═══════════════════════════════════════════════════════════════════"
echo "  Setting up GitHub repository: $REPO_NAME"
echo "═══════════════════════════════════════════════════════════════════"

# 1. Verify gh CLI is authenticated
echo ""
echo "→ Step 1/6: Checking gh CLI authentication..."
if ! gh auth status &> /dev/null; then
  echo "❌ ERROR: gh CLI not authenticated. Run: gh auth login"
  exit 1
fi
echo "  ✓ Authenticated as: $(gh api user --jq .login)"

# 2. Initialise git (skip if already initialised)
echo ""
echo "→ Step 2/6: Initialising git repository..."
if [ ! -d ".git" ]; then
  git init -b main
  echo "  ✓ Git initialised"
else
  echo "  ✓ Git already initialised"
fi

# 3. Configure user (use existing global config if present)
echo ""
echo "→ Step 3/6: Verifying git user configuration..."
GIT_NAME=$(git config user.name || echo "")
GIT_EMAIL=$(git config user.email || echo "")
if [ -z "$GIT_NAME" ] || [ -z "$GIT_EMAIL" ]; then
  echo "  ⚠ git user.name or user.email not set."
  echo "    Set them with:"
  echo "      git config --global user.name 'Hernando Grueso'"
  echo "      git config --global user.email 'hgrueso@unicef.org'"
  exit 1
fi
echo "  ✓ user.name : $GIT_NAME"
echo "  ✓ user.email: $GIT_EMAIL"

# 4. Stage files (relying on .gitignore to exclude raw data and outputs)
echo ""
echo "→ Step 4/6: Staging files..."
git add .gitignore || true
git add README.md REPLICATE.sh setup_git.sh 2>/dev/null || true
git add R/ 2>/dev/null || true
git add reports/nepal_slides_en.qmd reports/slides_theme.scss 2>/dev/null || true
git add reports/nepal_slides_en.html 2>/dev/null || true
git add reports/nepal_slides_en.pdf 2>/dev/null || true
echo "  ✓ Files staged"
git status --short | head -20

# 5. Initial commit
echo ""
echo "→ Step 5/6: Creating initial commit..."
if git log --oneline -1 &> /dev/null; then
  echo "  ⚠ Repository already has commits. Adding new commit instead..."
  git commit -m "Update: Nepal CoI analysis — slide deck and pipeline" || echo "  Nothing to commit."
else
  git commit -m "Initial commit: Nepal Cost of Inaction analysis

- R pipeline: data cleaning, regressions, causal forest, CoI estimation
- Slide deck: revealjs (HTML) and Beamer (PDF) policy-ready outputs
- Methodology: Mincer wage equations, GRF treatment effects, scaled CoI
- Source data: NLSS IV 2022/23 (not redistributed)"
  echo "  ✓ Initial commit created"
fi

# 6. Create remote repo and push
echo ""
echo "→ Step 6/6: Creating GitHub repository and pushing..."
if git remote get-url origin &> /dev/null; then
  echo "  ⚠ Remote 'origin' already exists. Pushing to existing remote..."
  git push -u origin main
else
  gh repo create "$REPO_NAME" \
    --"$VISIBILITY" \
    --description "$REPO_DESC" \
    --source=. \
    --remote=origin \
    --push
fi

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "  ✓ Repository ready: https://github.com/$(gh api user --jq .login)/$REPO_NAME"
echo "═══════════════════════════════════════════════════════════════════"
echo ""
echo "Next steps:"
echo "  • Add a license:   gh repo edit --add-topic 'license-mit'"
echo "  • Add topics:      gh repo edit --add-topic 'unicef,nepal,human-capital,nlss'"
echo "  • View on web:     gh repo view --web"

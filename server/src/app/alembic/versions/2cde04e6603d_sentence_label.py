"""sentence label

Revision ID: 2cde04e6603d
Revises: f0407e4e95d8
Create Date: 2022-04-27 22:16:16.489480

"""
from alembic import op
import sqlalchemy as sa
import sqlalchemy_utils


# revision identifiers, used by Alembic.
revision = '2cde04e6603d'
down_revision = 'f0407e4e95d8'
branch_labels = None
depends_on = None


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.add_column('sentence', sa.Column('label', sa.String(), nullable=True))
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('sentence', 'label')
    # ### end Alembic commands ###
